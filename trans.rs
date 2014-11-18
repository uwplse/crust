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

impl<T: Trans> Trans for P<T> {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        (**self).trans(tcx)
    }
}

impl Trans for FnDecl {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        let ret = match self.output {
            Return(ref t) => t.trans(tcx),
            NoReturn(_) => "bottom".into_string(),
        };
        format!("(args {}) return {}",
                self.inputs.trans(tcx),
                ret)
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
            _ => panic!("no ast_ty_to_ty_cache entry for {}", self),
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
                if let Some((var_name, var_idx)) = find_variant(tcx, &**func) {
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
                // TODO: nullary enum variants, static constants
                format!("var {}",
                        path.segments[path.segments.len() - 1].identifier.as_str())
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

fn find_variant(tcx: &ty::ctxt, expr: &Expr) -> Option<(String, uint)> {
    use rustc::middle::def::*;

    let def_map = tcx.def_map.borrow();

    let def = match def_map.get(&expr.id) {
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
        format!("{} : {}",
                self.pats[0].trans(tcx),
                self.body.trans(tcx))
    }
}

impl Trans for Pat {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        "[[Pat]]".into_string()
    }
}

struct TransVisitor<'a, 'tcx: 'a> {
    tcx: &'a ty::ctxt<'tcx>,
}

impl<'a, 'tcx, 'v> Visitor<'v> for TransVisitor<'a, 'tcx> {
    fn visit_fn(&mut self, fk: FnKind<'v>, fd: &'v FnDecl, b: &'v Block, s: Span, id: NodeId) {
        match fk {
            FkItemFn(name, generics, style, abi) => {
                let raw_name = name.as_str();
                println!("fn {} 0 0 {} body {} {{\n{}\t{}\n}}\n\n",
                         mangled_def_name(self.tcx, local_def(id)),
                         fd.trans(self.tcx),
                         match style {
                             UnsafeFn => "unsafe",
                             NormalFn => "block",
                         },
                         b.stmts.trans(self.tcx),
                         b.expr.as_ref().map(|e| e.trans(self.tcx))
                          .unwrap_or("unit simple_literal _".into_string()));
            },
            _ => {},
        }
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
