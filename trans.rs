use rustc::middle::ty;
use rustc::middle::typeck;
use rustc::util::ppaux::Repr;
use syntax::visit;
use syntax::visit::Visitor;
use syntax::visit::{FnKind, FkItemFn, FkMethod, FkFnBlock};
use syntax::ast::*;
use syntax::codemap::Span;
use syntax::ptr::P;
use syntax::ast_util::local_def;
use rustc::metadata::csearch;

trait Trans {
    fn trans(&self, tcx: &ty::ctxt) -> String;
}

impl<T: Trans> Trans for Vec<T> {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        let mut result = format!("{}", self.len());
        for item in self.iter() {
            result.push_str(" ");
            result.push_str(item.trans(tcx).as_slice());
        }
        result
    }
}

impl<'a, T: Trans> Trans for &'a [T] {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        let mut result = format!("{}", self.len());
        for item in self.iter() {
            result.push_str(" ");
            result.push_str(item.trans(tcx).as_slice());
        }
        result
    }
}

impl<T: Trans> Trans for P<T> {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        (**self).trans(tcx)
    }
}

impl Trans for String {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        self.clone()
    }
}

impl Trans for Ident {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        self.as_str().into_string()
    }
}

impl Trans for FunctionRetTy {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        match *self {
            Return(ref t) => t.trans(tcx),
            NoReturn(_) => "bottom".into_string(),
        }
    }
}


impl Trans for FnDecl {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        format!("(args {}) return {}",
                self.inputs.trans(tcx),
                self.output.trans(tcx))
    }
}

impl Trans for Arg {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        let name = match self.pat.node {
            PatIdent(_, span_ident, _) => {
                // TODO: check that span_ident doesn't refer to a nullary enum variant
                span_ident.node.as_str().into_string()
            },
            _ => panic!("unsupported Pat_ variant in Arg"),
        };
        format!("{} {}", name, self.ty.trans(tcx))
    }
}

impl Trans for Ty {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        match tcx.ast_ty_to_ty_cache.borrow().find(&self.id) {
            Some(&ty::atttce_resolved(t)) => t.trans(tcx),
            //_ => panic!("no ast_ty_to_ty_cache entry for {}", self),
            _ => format!("[[no_ty_to_ty {}]]", self.repr(tcx)),
        }
    }
}

impl Trans for ty::t {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        use rustc::middle::ty::sty::*;
        let s = match ty::get(*self).sty {
            ty_bool => "bool".into_string(),
            // ty_char
            ty_int(ity) => format!("int {}",
                                   match ity {
                                       TyI64 => 64u,
                                       TyI32 | TyI => 32,
                                       TyI16 => 16,
                                       TyI8 => 8,
                                   }),
            ty_uint(uty) => format!("uint {}",
                                    match uty {
                                        TyU64 => 64u,
                                        TyU32 | TyU => 32,
                                        TyU16 => 16,
                                        TyU8 => 8,
                                    }),
            // ty_float
            // TODO: handle substs
            ty_enum(did, ref substs) => format!("adt {} 0 0",
                                                mangled_def_name(tcx, did)),
            // ty_uniq
            // ty_str
            // ty_vec
            ty_ptr(mt) => format!("{} {}",
                                  match mt.mutbl {
                                      MutMutable => "ptr_mut",
                                      MutImmutable => "ptr",
                                  },
                                  mt.ty.trans(tcx)),
            ty_rptr(ref r, mt) => format!("{} {} {}",
                                          match mt.mutbl {
                                              MutMutable => "ref_mut",
                                              MutImmutable => "ref",
                                          },
                                          r.trans(tcx),
                                          mt.ty.trans(tcx)),
            ty_bare_fn(_) => "fn".into_string(),
            // ty_closure
            // ty_trait
            // TODO: handle substs
            ty_struct(did, ref substs) => format!("adt {} 0 0",
                                                  mangled_def_name(tcx, did)),
            // ty_unboxed_closure
            ty_tup(ref ts) if ts.len() == 0 => "unit".into_string(),
            ty_tup(ref ts) => format!("tuple {}", ts.trans(tcx)),
            ty_param(ref param) => "[[ty_param]]".into_string(),
            // ty_open
            // ty_infer
            // ty_err
            _ => panic!("unrecognized type: {}", self.repr(tcx)),

        };
        format!("[{}]", s)
    }
}

impl Trans for ty::Region {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        "[[Region]]".into_string()
    }
}

impl Trans for Block {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        format!("{} {{\n{}\t{}\n}}\n",
                match self.rules {
                    DefaultBlock => "block",
                    UnsafeBlock(_) => "unsafe",
                },
                self.stmts.trans(tcx),
                self.expr.as_ref().map(|e| e.trans(tcx))
                    .unwrap_or("unit simple_literal _".into_string()))
    }
}

impl Trans for Stmt {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        match self.node {
            StmtDecl(ref d, _id) => format!("\t{};\n", d.trans(tcx)),
            StmtExpr(ref e, _id) => format!("\texpr {};\n", e.trans(tcx)),
            StmtSemi(ref e, _id) => format!("\texpr {};\n", e.trans(tcx)),
            StmtMac(..) => panic!("expected no macros, but saw StmtMac"),
        }
    }
}

impl Trans for Decl {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        match self.node {
            DeclLocal(ref local) => local.trans(tcx),
            DeclItem(_) => panic!("unexpected DeclItem"),
        }
    }
}

impl Trans for Local {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        let name = match self.pat.node {
            PatIdent(_, span_ident, _) => {
                // TODO: check that span_ident doesn't refer to a nullary enum variant
                span_ident.node.as_str().into_string()
            },
            _ => panic!("unsupported Pat_ variant in Local"),
        };
        assert!(self.init.is_some());
        format!("let {} {} {}",
                name,
                tcx.node_types.borrow()[self.id].trans(tcx),
                self.init.as_ref().unwrap().trans(tcx))
    }
}

impl Trans for Expr {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        let variant = match self.node {
            // ExprBox
            // ExprVec
            ExprCall(ref func, ref args) => {
                if let Some((var_name, var_idx)) = find_variant(tcx, func.id) {
                    format!("enum_literal {} {} {}",
                            var_name,
                            var_idx,
                            args.trans(tcx))
                } else {
                    format!("call {} 0 0 {}", func.trans(tcx), args.trans(tcx))
                }
            },
            ExprMethodCall(name, ref tys, ref args) =>
                format!("[[ExprMethodCall {} {} {}]]",
                        name.node.as_str(),
                        tys.trans(tcx),
                        args.trans(tcx)),
            ExprTup(ref xs) if xs.len() == 0 => "simple_literal _".into_string(),
            ExprTup(ref xs) => format!("tuple_literal {}", xs.trans(tcx)),
            ExprBinary(op, ref a, ref b) => {
                match tcx.method_map.borrow().get(&typeck::MethodCall::expr(self.id)) {
                    Some(callee) => match callee.origin {
                        typeck::MethodStatic(did) => {
                            format!("[[ExprBinary: overloaded binop]]")
                        },
                        _ => panic!("bad origin for callee in ExprBinary"),
                    },
                    None => {
                        format!("binop {} {} {}",
                                op,
                                a.trans(tcx),
                                b.trans(tcx))
                    },
                }
            },
            ExprUnary(op, ref a) =>
                format!("[[ExprUnary {} {}]]",
                        op,
                        a.trans(tcx)),
            ExprLit(ref lit) =>
                format!("simple_literal {}", lit.trans(tcx)),
            ExprCast(ref e, ref ty) =>
                format!("cast {} {}",
                        e.trans(tcx),
                        ty.trans(tcx)),
            ExprIf(ref cond, ref then, ref opt_else) =>
                format!("[[ExprIf {} {} {}]]",
                        cond.trans(tcx),
                        then.trans(tcx),
                        opt_else.as_ref().map(|e| e.trans(tcx))),
            // ExprIfLet
            // ExprWhile
            // ExprWhileLet
            // ExprForLoop
            // ExprLoop
            ExprMatch(ref expr, ref arms, _src) =>
                format!("match {} {}",
                        expr.trans(tcx),
                        arms.trans(tcx)),
            // ExprFnBlock
            // ExprProc
            // ExprUnboxedFn
            ExprBlock(ref b) => b.trans(tcx),
            ExprAssign(ref l, ref r) =>
                format!("assign {} {}",
                        l.trans(tcx),
                        r.trans(tcx)),
            // ExprAssignOp
            ExprField(ref expr, field, _) =>
                format!("field {} {}",
                        expr.trans(tcx),
                        field.node.as_str()),
            // ExprTupField
            // ExprIndex
            // ExprSlice
            ExprPath(ref path) => {
                if let Some((var_name, var_idx)) = find_variant(tcx, self.id) {
                    format!("enum_literal {} {} 0",
                            var_name,
                            var_idx)
                } else {
                    use rustc::middle::def::*;
                    match tcx.def_map.borrow()[self.id] {
                        DefLocal(..) =>
                            format!("var {}",
                                    path.segments[path.segments.len() - 1]
                                        .identifier.as_str().into_string()),
                        d => format!("const {}",
                                     mangled_def_name(tcx, d.def_id())),
                    }
                }
            },
            ExprAddrOf(_mutbl, ref expr) =>
                format!("addr_of {}", expr.trans(tcx)),
            // ExprBreak
            // ExprAgain
            ExprRet(ref opt_expr) =>
                format!("return {}",
                        opt_expr.as_ref().map(|e| e.trans(tcx))
                                .unwrap_or("unit simple_literal _".into_string())),
            // ExprInlineAsm
            // ExprMac
            ExprStruct(ref name, ref fields, ref opt_base) => "[[ExprStruct]]".into_string(),
            // ExprRepeat
            ExprParen(ref expr) => expr.trans(tcx),
            _ => panic!("unrecognized Expr_ variant"),
        };

        format!("({} {})",
                tcx.node_types.borrow()[self.id].trans(tcx),
                variant)
    }
}

fn find_variant(tcx: &ty::ctxt, id: NodeId) -> Option<(String, uint)> {
    use rustc::middle::def::*;

    let def_map = tcx.def_map.borrow();

    let def = match def_map.get(&id) {
        None => return None,
        Some(d) => d,
    };

    match *def {
        DefVariant(enum_did, variant_did, _is_structure) => {
            let info = ty::enum_variant_with_id(tcx, enum_did, variant_did);
            Some((mangled_def_name(tcx, variant_did), info.disr_val as uint))
        },
        _ => None,
    }
}

impl Trans for Lit {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        match self.node {
            // LitStr
            // LitBinary
            LitByte(b) => format!("{}", b),
            // LitChar
            LitInt(i, _) => format!("{}", i),
            // LitFloat
            // LitFloatUnsuffixed
            LitBool(b) => format!("{}", b),
            _ => panic!("unrecognized Lit_ variant"),
        }
    }
}

impl Trans for Arm {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        assert!(self.pats.len() == 1);
        assert!(self.guard.is_none());
        format!("{{ {} >> {} }}",
                self.pats[0].trans(tcx),
                self.body.trans(tcx))
    }
}

impl Trans for Pat {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        let variant = match self.node {
            PatWild(PatWildSingle) => "wild".into_string(),
            PatIdent(_mode, name, None) => {
                if let Some((var_name, var_idx)) = find_variant(tcx, self.id) {
                    format!("enum {} {} 0",
                            var_name,
                            var_idx)
                } else {
                    use rustc::middle::def::*;
                    match tcx.def_map.borrow().get(&self.id) {
                        None | Some(&DefLocal(_)) => format!("var {}",
                                                             name.node.trans(tcx)),
                        Some(ref d) => format!("const {}",
                                               mangled_def_name(tcx, d.def_id())),
                    }
                }
            },
            PatEnum(ref path, Some(ref args)) => {
                let (var_name, var_idx) = find_variant(tcx, self.id)
                        .expect("couldn't find variant for enum pattern");
                format!("enum {} {} {}",
                        var_name,
                        var_idx,
                        args.trans(tcx))
            },
            PatTup(ref args) => format!("tuple {}", args.trans(tcx)),
            PatLit(ref expr) => expr.trans(tcx),
            _ => panic!("unhandled Pat_ variant"),
        };
        format!("({} {})",
                tcx.node_types.borrow()[self.id].trans(tcx),
                variant)
    }
}

impl Trans for StructField {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        format!("{} {}",
                self.node.ident().unwrap().as_str().into_string(),
                self.node.ty.trans(tcx))
    }
}

impl Trans for Variant {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        format!("{} {}",
                self.node.name.trans(tcx),
                self.node.kind.trans(tcx))
    }
}

impl Trans for VariantKind {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        match *self {
            TupleVariantKind(ref args) => args.trans(tcx),
            _ => panic!("unsupported VariantKind variant"),
        }
    }
}

impl Trans for VariantArg {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        self.ty.trans(tcx)
    }
}

struct TransVisitor<'a, 'tcx: 'a> {
    tcx: &'a ty::ctxt<'tcx>,
}

impl<'a, 'tcx, 'v> Visitor<'v> for TransVisitor<'a, 'tcx> {
    fn visit_item(&mut self, i: &'v Item) {
        match i.node {
            ItemStruct(ref def, ref g) => {
                assert!(def.ctor_id.is_none());
                println!("struct {} 0 0 {};",
                         mangled_def_name(self.tcx, local_def(i.id)),
                         def.fields.trans(self.tcx));
            },
            ItemEnum(ref def, ref g) => {
                println!("enum {} {}",
                         i.ident.trans(self.tcx),
                         def.variants.trans(self.tcx));
            },
            ItemFn(ref decl, style, _, ref generics, ref body) => {
                println!("fn {} 0 0 {} body {} {} {{\n{}\t{}\n}}\n\n",
                         mangled_def_name(self.tcx, local_def(i.id)),
                         decl.trans(self.tcx),
                         decl.output.trans(self.tcx),
                         match style {
                             UnsafeFn => "unsafe",
                             NormalFn => "block",
                         },
                         body.stmts.trans(self.tcx),
                         body.expr.as_ref().map(|e| e.trans(self.tcx))
                             .unwrap_or("unit simple_literal _".into_string()));
            },
            ItemImpl(ref g, ref trait_ref, ref self_ty, ref items) => {
                for item in items.iter() {
                    match *item {
                        MethodImplItem(ref method) => {
                            let (name, generics, _, exp_self, _, decl, body, _) = match method.node {
                                MethDecl(a, ref b, c, ref d, e, ref f, ref g, h) => (a, b, c, d, e, f, g, h),
                                MethMac(_) => panic!("unexpected MethMac"),
                            };
                            let mut arg_strs = vec![];

                            let self_arg = match exp_self.node {
                                SelfStatic => None,
                                SelfValue(ref name) =>
                                    Some(format!("{} {}",
                                                 name.trans(self.tcx),
                                                 self_ty.trans(self.tcx))),
                                SelfRegion(ref opt_lifetime, mutbl, ref name) =>
                                    Some(format!("{} {} {} {}",
                                                 name.trans(self.tcx),
                                                 match mutbl {
                                                     MutMutable => "ref_mut",
                                                     MutImmutable => "ref",
                                                 },
                                                 "[[Option<Lifetime>]]".into_string(),
                                                 self_ty.trans(self.tcx))),
                                SelfExplicit(ref ty, ref name) =>
                                    Some(format!("{} {}",
                                                 name.trans(self.tcx),
                                                 self_ty.trans(self.tcx))),
                            };
                            let offset = match self_arg {
                                Some(arg) => {
                                    arg_strs.push(arg);
                                    1
                                },
                                None => 0,
                            };

                            arg_strs.extend(decl.inputs.slice_from(offset).iter().map(|x| x.trans(self.tcx)));
                            println!("method {} (args {})",
                                     mangled_def_name(self.tcx, local_def(method.id)),
                                     arg_strs.trans(self.tcx));

                        },
                        TypeImplItem(_) => panic!("unsupported TypeImplItem"),
                    }
                }
            },
            _ => {},
        }

        visit::walk_item(self, i);
    }
}


fn mangled_def_name(tcx: &ty::ctxt, did: DefId) -> String {
    let mut name = String::new();
    if did.krate == LOCAL_CRATE {
        tcx.map.with_path(did.node, |mut elems| {
            for elem in elems {
                name.push_str(elem.name().as_str());
                name.push_str("_");
            }
        })
    } else {
        for elem in csearch::get_item_path(tcx, did).into_iter() {
            name.push_str(elem.name().as_str());
            name.push_str("_");
        }
    }
    name.pop();
    name
}


pub fn process(tcx: &ty::ctxt) {
    let krate = tcx.map.krate();
    let mut visitor = TransVisitor { tcx: tcx };
    visit::walk_crate(&mut visitor, krate);
}
