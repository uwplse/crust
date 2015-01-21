use rustc::metadata::csearch;
use rustc::middle::subst::ParamSpace::*;
use rustc::middle::subst::ParamSpace;
use rustc::middle::subst;
use rustc::middle::ty;
use rustc::middle::typeck::{MethodCall, MethodCallee, MethodOrigin};
use rustc::middle::typeck;
use rustc::util::ppaux::Repr;
use syntax::ast::*;
use syntax::ast_util::local_def;
use syntax::codemap::Span;
use syntax::ptr::P;
use syntax::visit::Visitor;
use syntax::visit::{FnKind, FkItemFn, FkMethod, FkFnBlock};
use syntax::visit;
use std::collections::HashSet;

trait Trans {
    fn trans(&self, tcx: &ty::ctxt) -> String;
}

trait TransExtra<E> {
    fn trans_extra(&self, tcx: &ty::ctxt, extra: E) -> String;
}

/*
impl<T: Trans, E> TransExtra<E> for T {
    fn trans_extra(&self, tcx: &ty::ctxt, _: E) -> String {
        self.trans(tcx)
    }
}
*/

/*
impl<T: TransExtra<()>> Trans for T {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        self.trans_extra(tcx, ())
    }
}
*/

impl<T: Trans> Trans for Option<T> {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        self.as_slice().trans(tcx)
    }
}

impl<T: Trans> Trans for Vec<T> {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        self.as_slice().trans(tcx)
    }
}

impl<T: TransExtra<E>, E: Copy> TransExtra<E> for Vec<T> {
    fn trans_extra(&self, tcx: &ty::ctxt, extra: E) -> String {
        self.as_slice().trans_extra(tcx, extra)
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

impl<'a, T: TransExtra<E>, E: Copy> TransExtra<E> for &'a [T] {
    fn trans_extra(&self, tcx: &ty::ctxt, extra: E) -> String {
        let mut result = format!("{}", self.len());
        for item in self.iter() {
            result.push_str(" ");
            result.push_str(item.trans_extra(tcx, extra).as_slice());
        }
        result
    }
}

impl<T: Trans> Trans for P<T> {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        (**self).trans(tcx)
    }
}

impl<T: TransExtra<E>, E> TransExtra<E> for P<T> {
    fn trans_extra(&self, tcx: &ty::ctxt, extra: E) -> String {
        (**self).trans_extra(tcx, extra)
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

impl Trans for Name {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        self.as_str().into_string()
    }
}

impl Trans for ParamSpace {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        match *self {
            TypeSpace => "t_".into_string(),
            SelfSpace => "s_".into_string(),
            AssocSpace => "a_".into_string(),
            FnSpace => "f_".into_string(),
        }
    }
}

impl TransExtra<ParamSpace> for Generics {
    fn trans_extra(&self, tcx: &ty::ctxt, space: ParamSpace) -> String {
        let mut lifetimes = vec![];
        for i in range(0, self.lifetimes.len()) {
            //lifetimes.push(format!("{}{}", space.trans(tcx), i));
            lifetimes.push(format!("r_named_0_{}", self.lifetimes[i].lifetime.id));
        }

        let mut ty_params = vec![];
        for i in range(0, self.ty_params.len()) {
            ty_params.push(format!("{}{}", space.trans(tcx), i));
        }

        format!("{} {}",
                lifetimes.trans(tcx),
                ty_params.trans(tcx))
    }
}

/*
impl TransExtra<ParamSpace> for LifetimeDef {
    fn trans_extra(&self, tcx: &ty::ctxt, space: ParamSpace) -> String {
        format!("{}{}",
                space.trans(tcx),
                self.lifetime.name.trans(tcx))
    }
}

impl TransExtra<ParamSpace> for TyParam {
    fn trans_extra(&self, tcx: &ty::ctxt, space: ParamSpace) -> String {
        format!("{}{}",
                space.trans(tcx),
                self.ident.trans(tcx))
    }
}
*/

impl Trans for subst::Substs {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        format!("{} {}",
                self.regions.trans(tcx),
                self.types.as_slice().trans(tcx))
    }
}

impl Trans for subst::RegionSubsts {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        match *self {
            subst::ErasedRegions => panic!("unsupported ErasedRegions"),
            subst::NonerasedRegions(ref regions) => regions.as_slice().trans(tcx),
        }
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
            ty_enum(did, ref substs) => format!("adt {} {}",
                                                mangled_def_name(tcx, did),
                                                substs.trans(tcx)),
            // ty_uniq
            ty_str => "str".into_string(),
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
            ty_struct(did, ref substs) => format!("adt {} {}",
                                                  mangled_def_name(tcx, did),
                                                  substs.trans(tcx)),
            // ty_unboxed_closure
            ty_tup(ref ts) if ts.len() == 0 => "unit".into_string(),
            ty_tup(ref ts) => format!("tuple {}", ts.trans(tcx)),
            ty_param(ref param) => {
                format!("var {}{}",
                        param.space.trans(tcx),
                        param.idx)
            },
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
        match *self {
            ty::ReEarlyBound(id, _space, _idx, _name) => {
                format!("r_named_0_{}", id)
                //format!("r{}{}", space.trans(tcx), idx)
            },
            ty::ReLateBound(binder_id, ref br) => br.trans_extra(tcx, Some(binder_id)),
            ty::ReFree(ref fr) => fr.bound_region.trans_extra(tcx, None),
            ty::ReStatic => "r_static".into_string(),
            ty::ReScope(id) => format!("r_scope_{}", id),
            _ => panic!("unsupported Region variant"),
        }
    }
}

impl TransExtra<Option<NodeId>> for ty::BoundRegion {
    fn trans_extra(&self, tcx: &ty::ctxt, binder_id: Option<NodeId>) -> String {
        match *self {
            ty::BrAnon(idx) => format!("r_anon_{}", idx),
            ty::BrNamed(did, _) =>
                format!("r_named_{}_{}", did.krate, did.node),
            /*
            ty::BrNamed(did, _) => {
                use syntax::ast_map::Node::*;
                // We know the region is in the function space.  We just need to find the index.
                match tcx.map.get(binder_id.expect("missing binder_id for BrNamed")) {
                    NodeItem(item) => println!("got item"),
                    NodeTraitItem(item) => println!("got trait item"),
                    NodeImplItem(item) => println!("got impl item"),
                    _ => println!("got other item!!"),
                }
                "[[BrNamed]]".into_string()
            },
            */
            _ => "[[bad BoundRegion]]".into_string(), //panic!("unsupported BoundRegion variant"),
        }
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
                    .unwrap_or("[unit] simple_literal _Block".into_string()))
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
            // TODO: handle inner items
            DeclItem(_) => "expr ([unit] simple_literal _DeclItem)".into_string(),
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

impl Trans for Field {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        format!("{} {}",
                self.ident.node.trans(tcx),
                self.expr.trans(tcx))
    }
}

impl Trans for Lifetime {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        use rustc::middle::resolve_lifetime::DefRegion::*;
        match *tcx.named_region_map.get(&self.id)
                  .expect("missing DefRegion") {
            DefStaticRegion => "r_static".into_string(),
            DefEarlyBoundRegion(_, _, id) |
            DefLateBoundRegion(_, _, id) =>
                format!("r_named_{}_{}", LOCAL_CRATE, id),
            DefFreeRegion(..) => panic!("unsupported DefFreeRegion"),
        }
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
                    let did = tcx.def_map.borrow()[func.id].def_id();
                    let name = mangled_def_name(tcx, did);
                    let substs = match tcx.item_substs.borrow().get(&func.id) {
                        Some(item_substs) => item_substs.substs.trans(tcx),
                        None => "0 0".into_string(),
                    };
                    format!("call {} {} {}",
                            name,
                            substs,
                            args.trans(tcx))
                }
            },
            ExprMethodCall(name, ref tys, ref args) => {
                let call = typeck::MethodCall::expr(self.id);
                let map = tcx.method_map.borrow();
                let callee = &map[call];
                let name = match callee.origin {
                    MethodOrigin::MethodStatic(did) => {
                        mangled_def_name(tcx, did)
                    },
                    _ => panic!("unsupported MethodOrigin variant"),
                };
                assert!(tys.len() == 0);
                format!("call {} {} {}",
                        name,
                        callee.substs.trans(tcx),
                        args.trans(tcx))
            },
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
            ExprUnary(op, ref a) => {
                match tcx.method_map.borrow().get(&typeck::MethodCall::expr(self.id)) {
                    Some(callee) => match callee.origin {
                        typeck::MethodStatic(did) => {
                            format!("[[ExprUnary: overloaded binop]]")
                        },
                        _ => panic!("bad origin for callee in ExprUnary"),
                    },
                    None => {
                        match op {
                            UnDeref => format!("deref {}", a.trans(tcx)),
                            _ => format!("unop {} {}",
                                         op,
                                         a.trans(tcx)),
                        }
                    },
                }
            },
            ExprLit(ref lit) =>
                format!("simple_literal {}", lit.trans(tcx)),
            ExprCast(ref e, ref ty) =>
                format!("cast {} {}",
                        e.trans(tcx),
                        ty.trans(tcx)),
            ExprIf(ref cond, ref then, ref opt_else) => {
                let ty = tcx.node_types.borrow()[then.id];

                // NB: `then` is a Block, but opt_else is `Option<Expr>`.
                format!("match {} 2 \
                        {{ ([bool] simple_literal true) >> ({} {}) }} \
                        {{ ([bool] simple_literal false) >> {} }}",
                        cond.trans(tcx),
                        ty.trans(tcx),
                        then.trans(tcx),
                        opt_else.as_ref().map_or("[unit] simple_literal _ExprIf".into_string(),
                                                 |e| e.trans(tcx)))
            },
                
                        /*
                format!("[[ExprIf {} {} {}]]",
                        cond.trans(tcx),
                        then.trans(tcx),
                        opt_else.as_ref().map(|e| e.trans(tcx))),
                        */
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
                        DefStruct(did) =>
                            format!("struct_literal 0"),
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
                                .unwrap_or("[unit] simple_literal _ExprRet".into_string())),
            // ExprInlineAsm
            // ExprMac
            ExprStruct(ref name, ref fields, ref opt_base) => {
                assert!(opt_base.is_none());
                format!("struct_literal {}", fields.trans(tcx))
            },
            // ExprRepeat
            ExprParen(ref expr) => expr.trans(tcx),
            _ => panic!("unrecognized Expr_ variant"),
        };

        let expr_ty = tcx.node_types.borrow()[self.id];
        let unadjusted = format!("({} {})",
                                 expr_ty.trans(tcx),
                                 variant);

        match tcx.adjustments.borrow().get(&self.id) {
            None => unadjusted,
            Some(adj) => adjust_expr(tcx, adj, self, unadjusted),
        }
    }
}

fn adjust_expr(tcx: &ty::ctxt,
               adj: &ty::AutoAdjustment,
               expr: &Expr,
               unadjusted: String) -> String {
    let mut result = unadjusted;
    let mut result_ty = tcx.node_types.borrow()[expr.id];

    match *adj {
        ty::AdjustDerefRef(ref adr) => {
            for i in range(0, adr.autoderefs) {
                let (new_result, new_result_ty) = deref_once(tcx, expr, i, result, result_ty);
                result = new_result;
                result_ty = new_result_ty;
            }

            match adr.autoref {
                None => {},
                Some(ty::AutoPtr(region, mutbl, ref autoref)) => {
                    assert!(autoref.is_none());
                    let mt = ty::mt { ty: result_ty, mutbl: mutbl };
                    result_ty = ty::mk_t(tcx, ty::ty_rptr(region, mt));
                    result = format!("({} addr_of {})",
                                     result_ty.trans(tcx),
                                     result);
                },
                Some(ty::AutoUnsafe(mutbl, ref autoref)) => {
                    assert!(autoref.is_none());
                    let mt = ty::mt { ty: result_ty, mutbl: mutbl };
                    result_ty = ty::mk_t(tcx, ty::ty_ptr(mt));
                    result = format!("({} addr_of {})",
                                     result_ty.trans(tcx),
                                     result);
                },
                _ => panic!("unsupported AutoRef variant"),
            }

            //assert!(adr.autoref.is_none());
        },
        ty::AdjustAddEnv(_) => panic!("unsupported AdjustAddEnv"),
    }

    result
}

fn deref_once(tcx: &ty::ctxt,
              expr: &Expr,
              level: uint,
              expr_str: String,
              expr_ty: ty::t) -> (String, ty::t) {
    match ty::get(expr_ty).sty {
        ty::ty_ptr(ty::mt { ty, .. }) |
        ty::ty_rptr(_, ty::mt { ty, .. }) => {
            let new_expr_str = format!("({} deref {})",
                                       ty.trans(tcx),
                                       expr_str);
            (new_expr_str, ty)
        },
        _ => panic!("unexpected ty variant"),
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
            // NB: For PatLit, we skip the code below that adds the pattern type.
            PatLit(ref expr) => return expr.trans(tcx),
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

impl Trans for ty::DtorKind {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        let opt = match *self {
            ty::NoDtor => None,
            ty::TraitDtor(did, _) => Some(mangled_def_name(tcx, did)),
        };
        opt.trans(tcx)
    }
}

impl Trans for VariantArg {
    fn trans(&self, tcx: &ty::ctxt) -> String {
        self.ty.trans(tcx)
    }
}

struct TransVisitor<'a, 'tcx: 'a> {
    tcx: &'a ty::ctxt<'tcx>,
    filter_fn: HashSet<String>
}

impl<'a, 'tcx, 'v> Visitor<'v> for TransVisitor<'a, 'tcx> {
    fn visit_item(&mut self, i: &'v Item) {
        println!("{}", i.trans_extra(self.tcx, &self.filter_fn));
        visit::walk_item(self, i);
    }
}

impl<'a> TransExtra<&'a HashSet<String>> for Item {
    fn trans_extra(&self, tcx: &ty::ctxt, filter_fn: &'a HashSet<String>) -> String {
        match self.node {
            ItemStruct(ref def, ref g) => {
                //assert!(def.ctor_id.is_none());
                format!("struct {} {} {} {};",
                        mangled_def_name(tcx, local_def(self.id)),
                        g.trans_extra(tcx, TypeSpace),
                        def.fields.trans(tcx),
                        ty::ty_dtor(tcx, local_def(self.id)).trans(tcx))
            },
            ItemEnum(ref def, ref g) => {
                format!("enum {} {} {} {};",
                        self.ident.trans(tcx),
                        g.trans_extra(tcx, TypeSpace),
                        def.variants.trans(tcx),
                        ty::ty_dtor(tcx, local_def(self.id)).trans(tcx))
            },
            ItemFn(ref decl, style, _, ref generics, ref body) => {
                let mangled_name = mangled_def_name(tcx, local_def(self.id));
                if filter_fn.contains(&mangled_name) {
                    format!("")
                } else {
                    format!("fn {} {} {} 0 body {} {} {{\n{}\t{}\n}}\n\n",
                            mangled_name,
                            generics.trans_extra(tcx, FnSpace),
                            decl.trans(tcx),
                            decl.output.trans(tcx),
                            match style {
                                UnsafeFn => "unsafe",
                                NormalFn => "block",
                            },
                            body.stmts.trans(tcx),
                            body.expr.as_ref().map(|e| e.trans(tcx))
                                .unwrap_or("[unit] simple_literal _ItemFn".into_string()))
                }
            },
            ItemImpl(ref impl_generics, ref trait_ref, ref self_ty, ref items) => {
                let mut result = String::new();
                for item in items.iter() {
                    let part = match *item {
                        MethodImplItem(ref method) => {
                            let mangled_name = mangled_def_name(tcx, local_def(method.id));
                            if filter_fn.contains(&mangled_name) {
                                return format!("");
                            };
                            let (name, generics, _, exp_self, style, decl, body, _) = match method.node {
                                MethDecl(a, ref b, c, ref d, e, ref f, ref g, h) => (a, b, c, d, e, f, g, h),
                                MethMac(_) => panic!("unexpected MethMac"),
                            };
                            let mut arg_strs = vec![];



                            let self_arg = match exp_self.node {
                                SelfStatic => None,
                                SelfValue(ref name) =>
                                    Some(format!("{} {}",
                                                 name.trans(tcx),
                                                 self_ty.trans(tcx))),
                                SelfRegion(ref opt_lifetime, mutbl, ref name) =>
                                    Some(format!("{} {} {} {}",
                                                 name.trans(tcx),
                                                 match mutbl {
                                                     MutMutable => "ref_mut",
                                                     MutImmutable => "ref",
                                                 },
                                                 match *opt_lifetime {
                                                     Some(ref lifetime) =>
                                                         lifetime.trans(tcx),
                                                     None => "r_anon_0".into_string(),
                                                 },
                                                 self_ty.trans(tcx))),
                                SelfExplicit(ref ty, ref name) =>
                                    Some(format!("{} {}",
                                                 name.trans(tcx),
                                                 self_ty.trans(tcx))),
                            };
                            let offset = match self_arg {
                                Some(arg) => {
                                    arg_strs.push(arg);
                                    1
                                },
                                None => 0,
                            };

                            let impl_clause = match *trait_ref {
                                Some(ref trait_ref) => {
                                    let last_seg = trait_ref.path.segments.as_slice().last().unwrap();
                                    let mut tys = vec![self_ty.trans(tcx)];
                                    let mut lifes = vec![];
                                    match last_seg.parameters {
                                        AngleBracketedParameters(ref params) => {
                                            for life in params.lifetimes.iter() {
                                                lifes.push(life.trans(tcx));
                                            }
                                            for ty in params.types.iter() {
                                                tys.push(ty.trans(tcx));
                                            }
                                        },
                                        ParenthesizedParameters(_) =>
                                            panic!("unsupported ParenthesizedParameters"),
                                    }

                                    format!("1 impl {}_{} {} {}",
                                            mangled_def_name(tcx, tcx.def_map.borrow()[trait_ref.ref_id].def_id()),
                                            name.trans(tcx),
                                            lifes.trans(tcx),
                                            tys.trans(tcx))
                                },
                                None => format!("0"),
                            };

                            let (lifetimes, ty_params) = combine_generics(tcx, impl_generics, generics);

                            arg_strs.extend(decl.inputs.slice_from(offset).iter().map(|x| x.trans(tcx)));
                            format!("fn {} {} {} (args {}) return {} {} body {} {} {{\n{}\t{}\n}}\n\n",
                                    mangled_name,
                                    lifetimes.trans(tcx),
                                    ty_params.trans(tcx),
                                    arg_strs.trans(tcx),
                                    decl.output.trans(tcx),
                                    impl_clause,
                                    decl.output.trans(tcx),
                                    match style {
                                        UnsafeFn => "unsafe",
                                        NormalFn => "block",
                                    },
                                    body.stmts.trans(tcx),
                                    body.expr.as_ref().map(|e| e.trans(tcx))
                                    .unwrap_or("[unit] simple_literal _method".into_string()))
                        },
                        TypeImplItem(_) => panic!("unsupported TypeImplItem"),
                    };
                    result.push_str(part.as_slice());
                    result.push_str("\n");
                }
                result
            },
            ItemConst(ref ty, ref expr) => {
                format!("const {} {} {}",
                        self.ident.trans(tcx),
                        ty.trans(tcx),
                        expr.trans(tcx))
            },
            _ => "".into_string(),
        }
    }
}

fn combine_generics(tcx: &ty::ctxt, impl_g: &Generics, fn_g: &Generics) -> (Vec<String>, Vec<String>) {
    let lifetimes =
            impl_g.lifetimes.iter().map(|l| format!("r_named_0_{}", l.lifetime.id)).chain(
            fn_g.lifetimes.iter().map(|l| format!("r_named_0_{}", l.lifetime.id))).collect();
    let ty_params =
            range(0, impl_g.ty_params.len()).map(|i| format!("t_{}", i)).chain(
            range(0, fn_g.ty_params.len()).map(|i| format!("f_{}", i))).collect();
    (lifetimes, ty_params)
}

fn clean_path_elem(s: &str, out: &mut String) {
    let mut depth = 0u;
    for c in s.chars() {
        if c == '<' {
            depth += 1;
        } else if c == '>' {
            depth -= 1;
        } else if depth == 0 {
            if c == '.' {
                out.push_str("__");
            } else {
                out.push(c);
            }
        }
    }
}

fn mangled_def_name(tcx: &ty::ctxt, did: DefId) -> String {
    let mut name = String::new();
    if did.krate == LOCAL_CRATE {
        tcx.map.with_path(did.node, |mut elems| {
            for elem in elems {
                clean_path_elem(elem.name().as_str(), &mut name);
                name.push_str("_");
            }
        })
    } else {
        for elem in csearch::get_item_path(tcx, did).into_iter() {
            clean_path_elem(elem.name().as_str(), &mut name);
            name.push_str("_");
        }
    }
    name.pop();
    name
}


pub fn process(tcx: &ty::ctxt, filter_fn : HashSet<String>) {
    let krate = tcx.map.krate();
    let mut visitor = TransVisitor { tcx: tcx, filter_fn: filter_fn };
    visit::walk_crate(&mut visitor, krate);
}
