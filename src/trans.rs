use std::any::Any;
use std::boxed::BoxAny;
use std::collections::{HashMap, HashSet};

use rustc::metadata::csearch;
use rustc::middle::astencode;
use rustc::middle::def;
use rustc::middle::region;
use rustc::middle::subst::ParamSpace::*;
use rustc::middle::subst::ParamSpace;
use rustc::middle::subst::{self, Subst};
use rustc::middle::ty;
use rustc::middle::ty::{MethodCall, MethodCallee, MethodOrigin};
//use rustc::middle::typeck;
use rustc::util::ppaux::Repr;
use syntax::ast::*;
use syntax::ast_map;
use syntax::ast_util;
use syntax::ast_util::local_def;
use syntax::codemap::{self, Span};
use syntax::ptr::P;
use syntax::visit::Visitor;
use syntax::visit::{FnKind, FkItemFn, FkMethod, FkFnBlock};
use syntax::visit;

struct TransCtxt<'a, 'tcx: 'a> {
    tcx: &'a ty::ctxt<'tcx>,
    observed_abstract_fns: HashMap<String, DefId>,
    observed_abstract_types: HashMap<String, DefId>,
    crate_name: String,
}

trait Trans {
    fn trans(&self, trcx: &mut TransCtxt) -> String;
}

trait TransExtra<E> {
    fn trans_extra(&self, trcx: &mut TransCtxt, extra: E) -> String;
}

/*
impl<T: Trans, E> TransExtra<E> for T {
    fn trans_extra(&self, trcx: &mut TransCtxt, _: E) -> String {
        self.trans(trcx)
    }
}
*/

/*
impl<T: TransExtra<()>> Trans for T {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        self.trans_extra(trcx, ())
    }
}
*/

impl<T: Trans> Trans for Option<T> {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        self.as_slice().trans(trcx)
    }
}

impl<T: Trans> Trans for Vec<T> {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        self.as_slice().trans(trcx)
    }
}

struct SliceIndex;

impl<T: TransExtra<usize>> TransExtra<SliceIndex> for Vec<T> {
    fn trans_extra(&self, trcx: &mut TransCtxt, _: SliceIndex) -> String {
        self.as_slice().trans_extra(trcx, SliceIndex)
    }
}

impl<T: TransExtra<E>, E: Copy> TransExtra<E> for Vec<T> {
    fn trans_extra(&self, trcx: &mut TransCtxt, extra: E) -> String {
        self.as_slice().trans_extra(trcx, extra)
    }
}

impl<'a, T: Trans> Trans for &'a [T] {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        let mut result = format!("{}", self.len());
        for item in self.iter() {
            result.push_str(" ");
            result.push_str(item.trans(trcx).as_slice());
        }
        result
    }
}

impl<'a, T: TransExtra<usize>> TransExtra<SliceIndex> for &'a [T] {
    fn trans_extra(&self, trcx: &mut TransCtxt, _: SliceIndex) -> String {
        let mut result = format!("{}", self.len());
        for (idx, item) in self.iter().enumerate() {
            result.push_str(" ");
            result.push_str(item.trans_extra(trcx, idx).as_slice());
        }
        result
    }
}

impl<'a, T: TransExtra<E>, E: Copy> TransExtra<E> for &'a [T] {
    fn trans_extra(&self, trcx: &mut TransCtxt, extra: E) -> String {
        let mut result = format!("{}", self.len());
        for item in self.iter() {
            result.push_str(" ");
            result.push_str(item.trans_extra(trcx, extra).as_slice());
        }
        result
    }
}

impl<T: Trans> Trans for P<T> {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        (**self).trans(trcx)
    }
}

impl<T: TransExtra<E>, E> TransExtra<E> for P<T> {
    fn trans_extra(&self, trcx: &mut TransCtxt, extra: E) -> String {
        (**self).trans_extra(trcx, extra)
    }
}

impl Trans for String {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        self.clone()
    }
}

impl Trans for Ident {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        format!("{}", self.as_str())
    }
}

impl Trans for Name {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        format!("{}", self.as_str())
    }
}

impl Trans for ParamSpace {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        format!("{}",
                match *self {
                    TypeSpace => "t_",
                    SelfSpace => "s_",
                    FnSpace => "f_",
                })
    }
}

impl TransExtra<ParamSpace> for Generics {
    fn trans_extra(&self, trcx: &mut TransCtxt, space: ParamSpace) -> String {
        let mut lifetimes = vec![];
        for i in range(0, self.lifetimes.len()) {
            //lifetimes.push(format!("{}{}", space.trans(trcx), i));
            lifetimes.push(format!("r_named_0_{}", self.lifetimes[i].lifetime.id));
        }

        let mut ty_params = vec![];
        for i in range(0, self.ty_params.len()) {
            ty_params.push(format!("{}{}", space.trans(trcx), i));
        }

        format!("{} {}",
                lifetimes.trans(trcx),
                ty_params.trans(trcx))
    }
}

/*
impl TransExtra<ParamSpace> for LifetimeDef {
    fn trans_extra(&self, trcx: &mut TransCtxt, space: ParamSpace) -> String {
        format!("{}{}",
                space.trans(trcx),
                self.lifetime.name.trans(trcx))
    }
}

impl TransExtra<ParamSpace> for TyParam {
    fn trans_extra(&self, trcx: &mut TransCtxt, space: ParamSpace) -> String {
        format!("{}{}",
                space.trans(trcx),
                self.ident.trans(trcx))
    }
}
*/

impl<'tcx> Trans for subst::Substs<'tcx> {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        format!("{} {}",
                self.regions.trans(trcx),
                self.types.as_slice().trans(trcx))
    }
}

impl Trans for subst::RegionSubsts {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        match *self {
            subst::ErasedRegions => panic!("unsupported ErasedRegions"),
            subst::NonerasedRegions(ref regions) => regions.as_slice().trans(trcx),
        }
    }
}

impl Trans for FunctionRetTy {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        match *self {
            Return(ref t) => t.trans(trcx),
            NoReturn(_) => format!("bottom"),
            DefaultReturn(_) => format!("unit"),
        }
    }
}


impl Trans for FnDecl {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        format!("(args {}) return {}",
                self.inputs.trans(trcx),
                self.output.trans(trcx))
    }
}

impl Trans for Arg {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        let ty_str = self.ty.trans(trcx);
        self.pat.trans_extra(trcx, ty_str)
    }
}

impl Trans for Ty {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        match trcx.tcx.ast_ty_to_ty_cache.borrow().get(&self.id) {
            Some(&ty::atttce_resolved(t)) => t.trans(trcx),
            //_ => panic!("no ast_ty_to_ty_cache entry for {}", self),
            _ => format!("[[no_ty_to_ty {}]]", self.repr(trcx.tcx)),
        }
    }
}

impl<'tcx> Trans for ty::Ty<'tcx> {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        use rustc::middle::ty::sty::*;
        let s = match self.sty {
            ty_bool => format!("bool"),
            ty_char => format!("char"),
            ty_int(TyIs(_)) => format!("int size"),
            ty_int(ity) => format!("int {}",
                                   match ity {
                                       TyI64 => 64us,
                                       TyI32 => 32,
                                       TyI16 => 16,
                                       TyI8 => 8,
                                       TyIs(_) => unreachable!(),
                                   }),
            ty_uint(TyUs(_)) => format!("uint size"),
            ty_uint(uty) => format!("uint {}",
                                    match uty {
                                        TyU64 => 64us,
                                        TyU32 => 32,
                                        TyU16 => 16,
                                        TyU8 => 8,
                                        TyUs(_) => unreachable!(),
                                    }),
            ty_float(fty) => format!("float {}",
                                     match fty {
                                         TyF64 => 64us,
                                         TyF32 => 32,
                                     }),
            // TODO: handle substs
            ty_enum(did, ref substs) => format!("adt {} {}",
                                                mangled_def_name(trcx, did),
                                                substs.trans(trcx)),
            // ty_uniq
            ty_str => format!("str"),
            ty_vec(ref ty, None) => format!("vec {}",
                                            ty.trans(trcx)),
            ty_vec(ref ty, Some(len)) => format!("fixed_vec {} {}",
                                                 len,
                                                 ty.trans(trcx)),
            ty_ptr(mt) => format!("{} {}",
                                  match mt.mutbl {
                                      MutMutable => "ptr_mut",
                                      MutImmutable => "ptr",
                                  },
                                  mt.ty.trans(trcx)),
            ty_rptr(ref r, mt) => format!("{} {} {}",
                                          match mt.mutbl {
                                              MutMutable => "ref_mut",
                                              MutImmutable => "ref",
                                          },
                                          r.trans(trcx),
                                          mt.ty.trans(trcx)),
            //ty_bare_fn(_, _) => format!("fn"),
            // ty_closure
            // ty_trait
            // TODO: handle substs
            ty_struct(did, ref substs) => format!("adt {} {}",
                                                  mangled_def_name(trcx, did),
                                                  substs.trans(trcx)),
            // ty_unboxed_closure
            ty_tup(ref ts) if ts.len() == 0 => format!("unit"),
            ty_tup(ref ts) => format!("tuple {}", ts.trans(trcx)),
            ty_projection(ref proj) => {
                let trait_did = proj.trait_ref.def_id;
                let name = format!("{}${}",
                                   mangled_def_name(trcx, proj.trait_ref.def_id),
                                   proj.item_name.trans(trcx));

                trcx.observed_abstract_types.insert(name.clone(), trait_did);

                format!("abstract {} {}",
                        name,
                        proj.trait_ref.substs.trans(trcx))
            },
            ty_param(ref param) => {
                format!("var {}{}",
                        param.space.trans(trcx),
                        param.idx)
            },
            // ty_open
            // ty_infer
            // ty_err
            _ => panic!("unrecognized type: {:?}", self),

        };
        format!("[{}]", s)
    }
}

impl Trans for ty::Region {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        match *self {
            ty::ReEarlyBound(id, _space, _idx, _name) => {
                format!("r_named_0_{}", id)
                //format!("r{}{}", space.trans(trcx), idx)
            },
            ty::ReLateBound(db_idx, ref br) => {
                br.trans_extra(trcx, None)
            },
            ty::ReFree(ref fr) => fr.bound_region.trans_extra(trcx, None),
            ty::ReStatic => format!("r_static"),
            ty::ReScope(extent) => {
                let region::CodeExtent::Misc(id) = extent;
                format!("r_scope_{}", id)
            },
            ty::ReEmpty => format!("empty"),
            _ => panic!("unsupported Region variant {:?}", *self),
        }
    }
}

impl TransExtra<Option<NodeId>> for ty::BoundRegion {
    fn trans_extra(&self, trcx: &mut TransCtxt, binder_id: Option<NodeId>) -> String {
        match *self {
            ty::BrAnon(idx) => format!("r_anon_{}", idx),
            ty::BrNamed(did, _) =>
                format!("r_named_{}_{}", did.krate, did.node),
            /*
            ty::BrNamed(did, _) => {
                use syntax::ast_map::Node::*;
                // We know the region is in the function space.  We just need to find the index.
                match trcx.tcx.map.get(binder_id.expect("missing binder_id for BrNamed")) {
                    NodeItem(item) => println!("got item"),
                    NodeTraitItem(item) => println!("got trait item"),
                    NodeImplItem(item) => println!("got impl item"),
                    _ => println!("got other item!!"),
                }
                "[[BrNamed]]".into_string()
            },
            */
            _ => panic!("unsupported BoundRegion variant"),
        }
    }
}

impl Trans for Block {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        format!("{} {{\n{}\t{}\n}}\n",
                match self.rules {
                    DefaultBlock => "block",
                    UnsafeBlock(_) => "unsafe",
                },
                self.stmts.trans(trcx),
                self.expr.as_ref().map(|e| e.trans(trcx))
                    .unwrap_or(format!("[unit] simple_literal _Block")))
    }
}

impl Trans for Stmt {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        match self.node {
            StmtDecl(ref d, _id) => format!("\t{};\n", d.trans(trcx)),
            StmtExpr(ref e, _id) => format!("\texpr {};\n", e.trans(trcx)),
            StmtSemi(ref e, _id) => format!("\texpr {};\n", e.trans(trcx)),
            StmtMac(..) => panic!("expected no macros, but saw StmtMac"),
        }
    }
}

impl Trans for Decl {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        match self.node {
            DeclLocal(ref local) => local.trans(trcx),
            // TODO: handle inner items
            DeclItem(_) => format!("expr ([unit] simple_literal _DeclItem)"),
        }
    }
}

impl Trans for Local {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        format!("let {} {}",
                self.pat.trans(trcx),
                self.init.trans(trcx))
    }
}

impl Trans for Field {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        format!("{} {}",
                self.ident.node.trans(trcx),
                self.expr.trans(trcx))
    }
}

impl Trans for Lifetime {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        use rustc::middle::resolve_lifetime::DefRegion::*;
        match *trcx.tcx.named_region_map.get(&self.id)
                  .expect("missing DefRegion") {
            DefStaticRegion => format!("r_static"),
            DefEarlyBoundRegion(_, _, id) |
            DefLateBoundRegion(_, id) =>
                format!("r_named_{}_{}", LOCAL_CRATE, id),
            DefFreeRegion(..) => panic!("unsupported DefFreeRegion"),
        }
    }
}

fn trans_method_call(trcx: &mut TransCtxt,
                     callee: &MethodCallee,
                     args: Vec<String>) -> String {
    let name = match callee.origin {
        MethodOrigin::MethodStatic(did) => {
            mangled_def_name(trcx, did)
        },
        MethodOrigin::MethodTypeParam(ref mp) => {
            let trait_did = mp.trait_ref.def_id;
            // trait_ref substs are actually the same as the callee substs, so we can
            // ignore them here.
            let item_id = trcx.tcx.trait_item_def_ids.borrow()[trait_did][mp.method_num];
            let method_did = match item_id {
                ty::ImplOrTraitItemId::MethodTraitItemId(did) => did,
                ty::ImplOrTraitItemId::TypeTraitItemId(_) =>
                    panic!("unexpected TypeTraitItemId in method call"),
            };
            let name = mangled_def_name(trcx, method_did);
            trcx.observed_abstract_fns.insert(name.clone(), method_did);
            name
        },
        _ => panic!("unsupported MethodOrigin variant"),
    };
    format!("call {} {} {}",
            name,
            callee.substs.trans(trcx),
            args.trans(trcx))
}

impl Trans for Expr {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        let mut add_ty = true;

        let variant = match self.node {
            // ExprBox
            // ExprVec
            ExprCall(ref func, ref args) => {
                if let Some(struct_name) = find_tuple_struct_ctor(trcx, func.id) {
                    let mut fields = Vec::new();
                    for (i, a) in args.iter().enumerate() {
                        fields.push(format!("field{} {}", i, a.trans(trcx)));
                    }
                    format!("struct_literal {}", fields.trans(trcx))
                } else if let Some((var_name, var_idx)) = find_variant(trcx, func.id) {
                    format!("enum_literal {} {} {}",
                            var_name,
                            var_idx,
                            args.trans(trcx))
                } else {
                    let (did, is_abstract) = match trcx.tcx.def_map.borrow()[func.id] {
                        def::DefStaticMethod(did, prov) => match prov {
                            def::MethodProvenance::FromTrait(_) => (did, true),
                            _ => (did, false),
                        },
                        def => (def.def_id(), false),
                    };
                    let name = mangled_def_name(trcx, did);
                    let substs = match trcx.tcx.item_substs.borrow().get(&func.id) {
                        Some(item_substs) => item_substs.substs.trans(trcx),
                        None => format!("0 0"),
                    };
                    if is_abstract {
                        trcx.observed_abstract_fns.insert(name.clone(), did);
                    }
                    format!("call {} {} {}",
                            name,
                            substs,
                            args.trans(trcx))
                }
            },
            ExprMethodCall(name, ref tys, ref args) => {
                let call = MethodCall::expr(self.id);
                let map = trcx.tcx.method_map.borrow();
                let callee = &map[call];
                let arg_strs = args.iter().map(|x| x.trans(trcx)).collect();
                assert!(tys.len() == 0); // no idea what `tys` does
                trans_method_call(trcx, callee, arg_strs)
            },
            ExprTup(ref xs) if xs.len() == 0 => format!("simple_literal _"),
            ExprTup(ref xs) => format!("tuple_literal {}", xs.trans(trcx)),
            ExprBinary(op, ref a, ref b) => {
                match trcx.tcx.method_map.borrow().get(&MethodCall::expr(self.id)) {
                    Some(callee) => {
                        let b_str = if ast_util::is_by_value_binop(op.node) {
                            b.trans(trcx)
                        } else {
                            let unadjusted = b.trans(trcx);
                            do_auto_ref(trcx, &**b, unadjusted)
                        };
                        let arg_strs = vec![a.trans(trcx), b_str];
                        trans_method_call(trcx, callee, arg_strs)
                    },
                    None => {
                        format!("binop {:?} {} {}",
                                op.node,
                                a.trans(trcx),
                                b.trans(trcx))
                    },
                }
            },
            ExprUnary(op, ref a) => {
                match trcx.tcx.method_map.borrow().get(&MethodCall::expr(self.id)) {
                    Some(callee) => {
                        let arg_strs = vec![a.trans(trcx)];
                        let meth_call = trans_method_call(trcx, callee, arg_strs);
                        match op {
                            UnDeref => format!("deref ([ref r_dummy {}] {})", trcx.tcx.node_types.borrow()[self.id].trans(trcx), meth_call),
                            _ => meth_call
                        }
                    },
                    None => {
                        match op {
                            UnDeref => format!("deref {}", a.trans(trcx)),
                            _ => format!("unop {:?} {}",
                                         op,
                                         a.trans(trcx)),
                        }
                    },
                }
            },
            ExprLit(ref lit) =>
                format!("simple_literal {}", lit.trans(trcx)),
            ExprCast(ref e, _) =>
                format!("cast {}", e.trans(trcx)),
            ExprIf(ref cond, ref then, ref opt_else) => {
                let ty = trcx.tcx.node_types.borrow()[then.id];

                // NB: `then` is a Block, but opt_else is `Option<Expr>`.
                format!("match {} 2 \
                        {{ ([bool] simple_literal true) >> ({} {}) }} \
                        {{ ([bool] simple_literal false) >> {} }}",
                        cond.trans(trcx),
                        ty.trans(trcx),
                        then.trans(trcx),
                        opt_else.as_ref().map_or(format!("[unit] simple_literal _ExprIf"),
                                                 |e| e.trans(trcx)))
            },
                
                        /*
                format!("[[ExprIf {} {} {}]]",
                        cond.trans(trcx),
                        then.trans(trcx),
                        opt_else.as_ref().map(|e| e.trans(trcx))),
                        */
            // ExprIfLet
            // ExprWhile
            ExprWhile(ref guard, ref body, _) =>
                format!("while {} [unit] {}",
                        guard.trans(trcx),
                        body.trans(trcx)),
            // ExprWhileLet
            ExprForLoop(ref patt, ref expr, ref body, _ident) =>
                format!("for {} {} {}",
                        patt.trans(trcx),
                        expr.trans(trcx),
                        body.trans(trcx)),
            // ExprLoop
            ExprMatch(ref expr, ref arms, _src) =>
                format!("match {} {}",
                        expr.trans(trcx),
                        arms.trans(trcx)),
            // ExprFnBlock
            // ExprProc
            // ExprUnboxedFn
            ExprBlock(ref b) => b.trans(trcx),
            ExprAssign(ref l, ref r) =>
                format!("assign {} {}",
                        l.trans(trcx),
                        r.trans(trcx)),
            ExprAssignOp(ref op, ref rhs, ref operand) =>
                format!("assign_op {:?} {} {}",
                        op.node,
                        rhs.trans(trcx),
                        operand.trans(trcx)
                        ),
            ExprField(ref expr, field) =>
                format!("field {} {}",
                        expr.trans(trcx),
                        field.node.as_str()),
            ExprTupField(ref expr, index) =>
                format!("field {} field{}",
                        expr.trans(trcx),
                        index.node),
            ExprIndex(ref arr, ref idx) => {
                match trcx.tcx.method_map.borrow().get(&MethodCall::expr(self.id)) {
                    Some(callee) => {
                        let idx_ty = trcx.tcx.node_types.borrow()[idx.id];
                        let idx_str = format!("({} addr_of {})",
                                              auto_ref_ty(trcx, &**idx, None, idx_ty)
                                                  .trans(trcx),
                                              idx.trans(trcx));
                        let arg_strs = vec![arr.trans(trcx), idx_str];
                        let call_str = trans_method_call(trcx, callee, arg_strs);

                        let mutbl = match ty::ty_fn_ret(callee.ty).0 {
                            ty::FnConverging(ty) => match ty.sty {
                                ty::ty_rptr(_, mt) => mt.mutbl,
                                _ => panic!("unexpected ty variant"),
                            },
                            ty::FnDiverging => panic!("unexpected FnDiverging"),
                        };
                        let result_ty = trcx.tcx.node_types.borrow()[idx.id];
                        let result_ptr_ty = auto_ref_ty(trcx, self, Some(mutbl), result_ty);
                        format!("deref ({} {})",
                                result_ptr_ty.trans(trcx),
                                call_str)
                    },
                    None => {
                        format!("index {} {}",
                                arr.trans(trcx),
                                idx.trans(trcx))
                    },
                }
            },
            ExprRange(ref opt_low, ref opt_high) => {
                format!("range {} {}", opt_low.trans(trcx), opt_high.trans(trcx))
            },
            ExprPath(ref path) => {
                if let Some((var_name, var_idx)) = find_variant(trcx, self.id) {
                    format!("enum_literal {} {} 0",
                            var_name,
                            var_idx)
                } else {
                    use rustc::middle::def::*;
                    match trcx.tcx.def_map.borrow()[self.id] {
                        DefLocal(..) =>
                            format!("var {}",
                                    path.segments[path.segments.len() - 1]
                                        .identifier.as_str()),
                        DefStruct(did) =>
                            format!("struct_literal 0"),
                        DefStatic(did, _) =>
                            format!("var {}", mangled_def_name(trcx, did)),
                        d => format!("const {}",
                                     mangled_def_name(trcx, d.def_id())),
                    }
                }
            },
            ExprAddrOf(_mutbl, ref expr) =>
                format!("addr_of {}", expr.trans(trcx)),
            // ExprBreak
            // ExprAgain
            ExprRet(ref opt_expr) =>
                format!("return {}",
                        opt_expr.as_ref().map(|e| e.trans(trcx))
                                .unwrap_or(format!("[unit] simple_literal _ExprRet"))),
            // ExprInlineAsm
            // ExprMac
            ExprStruct(ref name, ref fields, ref opt_base) => {
                assert!(opt_base.is_none());
                format!("struct_literal {}", fields.trans(trcx))
            },
            // ExprRepeat
            ExprParen(ref expr) => {
                add_ty = false;
                expr.trans(trcx)
            },
            ExprVec(ref expr_list) => {
                format!("vec {}", expr_list.trans(trcx))
            },
            _ => panic!("unrecognized Expr_ variant"/* {:?}", self.node*/),
        };

        let expr_ty = trcx.tcx.node_types.borrow()[self.id];
        let unadjusted = 
                if add_ty {
                    format!("({} {})",
                            expr_ty.trans(trcx),
                            variant)
                } else {
                    variant
                };

        match trcx.tcx.adjustments.borrow().get(&self.id) {
            None => unadjusted,
            Some(adj) => adjust_expr(trcx, adj, self, unadjusted),
        }
    }
}

fn adjust_expr<'a, 'tcx>(trcx: &mut TransCtxt<'a, 'tcx>,
                         adj: &ty::AutoAdjustment<'tcx>,
                         expr: &Expr,
                         unadjusted: String) -> String {
    let mut result = unadjusted;
    let mut result_ty = trcx.tcx.node_types.borrow()[expr.id];

    match *adj {
        ty::AdjustDerefRef(ref adr) => {
            for i in range(0, adr.autoderefs) {
                let (new_result, new_result_ty) = deref_once(trcx, expr, i, result, result_ty);
                result = new_result;
                result_ty = new_result_ty;
            }

            fn go_autoref<'a, 'tcx>(trcx: &mut TransCtxt<'a, 'tcx>,
                                    autoref: &ty::AutoRef<'tcx>,
                                    mut result: String,
                                    mut result_ty: ty::Ty<'tcx>) -> (String, ty::Ty<'tcx>) {
                match *autoref {
                    ty::AutoPtr(region, mutbl, ref next_autoref) => {
                        if let Some(ref ar) = *next_autoref {
                            let (new_result, new_result_ty) =
                                    go_autoref(trcx, &**ar, result, result_ty);
                            result = format!("(( {} ))", new_result);
                            result_ty = new_result_ty;
                        }

                        let mt = ty::mt { ty: result_ty, mutbl: mutbl };
                        result_ty = ty::mk_t(trcx.tcx, ty::ty_rptr(trcx.tcx.mk_region(region), mt));
                        result = format!("({} addr_of {})",
                                         result_ty.trans(trcx),
                                         result);
                    },

                    ty::AutoUnsafe(mutbl, ref next_autoref) => {
                        if let Some(ref ar) = *next_autoref {
                            let (new_result, new_result_ty) =
                                    go_autoref(trcx, &**ar, result, result_ty);
                            result = format!("(( {} ))", new_result);
                            result_ty = new_result_ty;
                        }

                        let mt = ty::mt { ty: result_ty, mutbl: mutbl };
                        result_ty = ty::mk_t(trcx.tcx, ty::ty_ptr(mt));
                        result = format!("({} addr_of {})",
                                         result_ty.trans(trcx),
                                         result);
                    },

                    ty::AutoUnsize(ref unsize) => {
                        match *unsize {
                            ty::UnsizeLength(len) => {
                                result_ty = match result_ty.sty {
                                    ty::ty_vec(item_ty, _) =>
                                        ty::mk_vec(trcx.tcx, item_ty, None),
                                    _ => panic!("UnsizeLength of non-ty_vec"),
                                };
                                result = format!("({} unsize_len {} {})",
                                                 result_ty.trans(trcx),
                                                 len,
                                                 result);
                            },

                            _ => panic!("unsupported UnsizeKind variant"),
                        }
                    },

                    _ => panic!("unsupported AutoRef variant"),
                }

                (result, result_ty)
            }

            if let Some(ref autoref) = adr.autoref {
                let (new_result, new_result_ty) = go_autoref(trcx, autoref, result, result_ty);
                result = new_result;
                result_ty = new_result_ty;
            }
        },
        ty::AdjustReifyFnPointer(_) => {}, //panic!("unsupported AdjustAddEnv"),
    }

    result
}

fn auto_ref_ty<'a, 'tcx>(trcx: &mut TransCtxt<'a, 'tcx>,
                         expr: &Expr,
                         mutbl: Option<Mutability>,
                         ty: ty::Ty<'tcx>) -> ty::Ty<'tcx> {
    let region = ty::ReScope(region::CodeExtent::Misc(expr.id));
    let mt = ty::mt { ty: ty, mutbl: mutbl.unwrap_or(MutImmutable) };
    ty::mk_t(trcx.tcx, ty::ty_rptr(trcx.tcx.mk_region(region), mt))
}

fn do_auto_ref(trcx: &mut TransCtxt,
               expr: &Expr,
               unadjusted: String) -> String {
    let result = unadjusted;
    let mut result_ty = trcx.tcx.node_types.borrow()[expr.id];
    result_ty = auto_ref_ty(trcx, expr, None, result_ty);

    format!("({} addr_of {})",
            result_ty.trans(trcx),
            result)
}

fn deref_once<'a, 'tcx>(trcx: &mut TransCtxt<'a, 'tcx>,
                   expr: &Expr,
                   level: usize,
                   expr_str: String,
                   expr_ty: ty::Ty<'tcx>) -> (String, ty::Ty<'tcx>) {
    let (expr_str, expr_ty) =
        if let Some(callee) = trcx.tcx.method_map.borrow().get(&ty::MethodCall::autoderef(expr.id, level)) {
            let arg_str = do_auto_ref(trcx, expr, expr_str);
            let new_expr_variant = trans_method_call(trcx, callee, vec![arg_str]);

            let method_ret = match ty::ty_fn_ret(callee.ty).0 {
                ty::FnConverging(ty) => ty,
                ty::FnDiverging => panic!("unexpected FnDiverging"),
            };
            let adj = ty::AdjustDerefRef(ty::AutoDerefRef {
                autoderefs: 1,
                autoref: None,
            });
            let deref_ty = ty::adjust_ty(trcx.tcx,
                                         codemap::DUMMY_SP,
                                         expr.id,
                                         expr_ty,
                                         Some(&adj),
                                         |_| Some(callee.ty));
            let mutbl = match method_ret.sty {
                ty::ty_rptr(_, mt) => mt.mutbl,
                _ => panic!("unexpected ty variant"),
            };
            let new_expr_ty = auto_ref_ty(trcx, expr, Some(mutbl), deref_ty);

            (format!("({} {})", new_expr_ty.trans(trcx), new_expr_variant), new_expr_ty)
        } else {
            (expr_str, expr_ty)
        };

    match expr_ty.sty {
        ty::ty_ptr(ty::mt { ty, .. }) |
        ty::ty_rptr(_, ty::mt { ty, .. }) => {
            let new_expr_str = format!("({} deref {})",
                                       ty.trans(trcx),
                                       expr_str);
            (new_expr_str, ty)
        },
        _ => panic!("unexpected ty variant: {}", expr_ty.repr(trcx.tcx)),
    }
}

fn find_tuple_struct_ctor(trcx: &mut TransCtxt, id: NodeId) -> Option<String> {
    use rustc::middle::def::*;

    let def_map = trcx.tcx.def_map.borrow();

    let def = match def_map.get(&id) {
        None => return None,
        Some(d) => d,
    };

    match *def {
        DefStruct(struct_did) => {
            Some(mangled_def_name(trcx, struct_did))
        },
        _ => None,
    }
}

fn find_variant(trcx: &mut TransCtxt, id: NodeId) -> Option<(String, usize)> {
    use rustc::middle::def::*;

    let def_map = trcx.tcx.def_map.borrow();

    let def = match def_map.get(&id) {
        None => return None,
        Some(d) => d,
    };

    match *def {
        DefVariant(enum_did, variant_did, _is_structure) => {
            let info = ty::enum_variant_with_id(trcx.tcx, enum_did, variant_did);
            Some((mangled_def_name(trcx, variant_did), info.disr_val as usize))
        },
        _ => None,
    }
}

impl Trans for Lit {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        match self.node {
            LitStr(ref s, ref style) => {
                let mut result = String::from_str("str_");
                for b in s.get().bytes() {
                    result.push_str(&*format!("{:02x}", b));
                }
                result
            },
            // LitBinary
            LitByte(b) => format!("{}", b),
            LitChar(c) => format!("{}", c as u32),
            LitInt(i, _) => format!("{}", i),
            // LitFloat
            // LitFloatUnsuffixed
            LitBool(b) => format!("{}", b),
            _ => panic!("unrecognized Lit_ variant"),
        }
    }
}

impl Trans for Arm {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        assert!(self.pats.len() == 1);
        assert!(self.guard.is_none());
        format!("{{ {} >> {} }}",
                self.pats[0].trans(trcx),
                self.body.trans(trcx))
    }
}

impl TransExtra<String> for Pat {
    fn trans_extra(&self, trcx: &mut TransCtxt, ty_str: String) -> String {
        let variant = match self.node {
            PatWild(PatWildSingle) => format!("wild"),
            PatIdent(mode, name, None) => {
                if let Some((var_name, var_idx)) = find_variant(trcx, self.id) {
                    format!("enum {} {} 0",
                            var_name,
                            var_idx)
                } else {
                    use rustc::middle::def::*;
                    match trcx.tcx.def_map.borrow().get(&self.id) {
                        None | Some(&DefLocal(_)) => 
                            match mode {
                                BindByRef(_) => format!("ref_var {}", name.node.trans(trcx)),
                                BindByValue(_) => format!("var {}", name.node.trans(trcx)),
                            },
                        Some(ref d) => format!("const {}",
                                               mangled_def_name(trcx, d.def_id())),
                    }
                }
            },
            PatEnum(ref path, Some(ref args)) => {
                if let Some((var_name, var_idx)) = find_variant(trcx, self.id) {
                    format!("enum {} {} {}",
                            var_name,
                            var_idx,
                            args.trans(trcx))
                } else if let Some(struct_name) = find_tuple_struct_ctor(trcx, self.id) {
                    format!("tuple {}", args.trans(trcx))
                } else {
                    panic!("couldn't find enum variant or tuple struct for {}", path.repr(trcx.tcx));
                }
            },
            PatTup(ref args) => format!("tuple {}", args.trans(trcx)),
            PatRegion(ref pat, _mutbl) => format!("addr_of {}", pat.trans(trcx)),
            // NB: For PatLit, we skip the code below that adds the pattern type.
            PatLit(ref expr) => return expr.trans(trcx),
            _ => panic!("unhandled Pat_ variant"),
        };

        format!("({} {})",
                ty_str,
                variant)
    }
}

impl Trans for Pat {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        let ty_str = trcx.tcx.node_types.borrow()[self.id].trans(trcx);
        self.trans_extra(trcx, ty_str)
    }
}

impl TransExtra<usize> for StructField {
    fn trans_extra(&self, trcx: &mut TransCtxt, idx: usize) -> String {
        match self.node.ident() {
            Some(ident) => format!("{} {}",
                                   ident.trans(trcx),
                                   self.node.ty.trans(trcx)),
            None => format!("field{} {}",
                            idx,
                            self.node.ty.trans(trcx)),
        }
    }
}

impl Trans for Variant {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        format!("{} {}",
                mangled_def_name(trcx, local_def(self.node.id)),
                self.node.kind.trans(trcx))
    }
}

impl Trans for VariantKind {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        match *self {
            TupleVariantKind(ref args) => args.trans(trcx),
            _ => panic!("unsupported VariantKind variant"),
        }
    }
}

impl Trans for ty::DtorKind {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        let opt = match *self {
            ty::NoDtor => None,
            ty::TraitDtor(did, _) => Some(mangled_def_name(trcx, did)),
        };
        opt.trans(trcx)
    }
}

impl Trans for Visibility {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        match *self {
            Public => format!("pub"),
            Inherited => format!("priv"),
        }
    }
}

impl Trans for VariantArg {
    fn trans(&self, trcx: &mut TransCtxt) -> String {
        self.ty.trans(trcx)
    }
}

struct TransVisitor<'b, 'a: 'b, 'tcx: 'a> {
    trcx: &'b mut TransCtxt<'a, 'tcx>,
    filter_fn: HashSet<String>
}

fn try_str<F: FnOnce() -> String>(f: F, what: &str) -> String {
    let mut opt_str = None;
    let mut opt_f = Some(f);
    let result = unsafe {
        ::std::rt::unwind::try(|| {
            let f = opt_f.take().unwrap();
            opt_str = Some(f());
        })
    };
    match result {
        Ok(()) => {
            opt_str.unwrap()
        },
        Err(e) => {
            fn read(mut e: Box<Any>) -> String {
                match e.downcast::<String>() {
                    Ok(msg) => return *msg,
                    Err(e_) => e = e_,
                }

                match e.downcast::<&'static str>() {
                    Ok(msg) => return String::from_str(*msg),
                    Err(e_) => e = e_,
                }

                format!("(unknown error type: {:?}", e.get_type_id())
            }
            format!("# error with {}: {}", what, read(e))
        },
    }
}

impl<'b, 'a, 'tcx, 'v> Visitor<'v> for TransVisitor<'b, 'a, 'tcx> {
    fn visit_item(&mut self, i: &'v Item) {
        let name = mangled_def_name(self.trcx, local_def(i.id));
        let s = try_str(|| i.trans_extra(self.trcx, &self.filter_fn),
                        &*name);
        println!("{}", s);
        visit::walk_item(self, i);
    }
}

impl<'a> TransExtra<&'a HashSet<String>> for Item {
    fn trans_extra(&self, trcx: &mut TransCtxt, filter_fn: &'a HashSet<String>) -> String {
        match self.node {
            ItemStruct(ref def, ref g) => {
                format!("struct {} {} {} {};",
                        mangled_def_name(trcx, local_def(self.id)),
                        g.trans_extra(trcx, TypeSpace),
                        def.fields.trans_extra(trcx, SliceIndex),
                        ty::ty_dtor(trcx.tcx, local_def(self.id)).trans(trcx))
            },
            ItemEnum(ref def, ref g) => {
                format!("enum {} {} {} {};",
                        mangled_def_name(trcx, local_def(self.id)),
                        g.trans_extra(trcx, TypeSpace),
                        def.variants.trans(trcx),
                        ty::ty_dtor(trcx.tcx, local_def(self.id)).trans(trcx))
            },
            ItemFn(ref decl, style, _, ref generics, ref body) => {
                let mangled_name = mangled_def_name(trcx, local_def(self.id));
                if filter_fn.contains(&mangled_name) {
                    format!("")
                } else {
                    format!("fn {} {} {} {} 0 body {} {} {{\n{}\t{}\n}}\n\n",
                            self.vis.trans(trcx),
                            mangled_name,
                            generics.trans_extra(trcx, FnSpace),
                            decl.trans(trcx),
                            decl.output.trans(trcx),
                            match style {
                                Unsafety::Unsafe => "unsafe",
                                Unsafety::Normal => "block",
                            },
                            body.stmts.trans(trcx),
                            body.expr.as_ref().map(|e| e.trans(trcx))
                                .unwrap_or(format!("[unit] simple_literal _ItemFn")))
                }
            },

            ItemImpl(_, _, ref impl_generics, ref trait_ref, ref self_ty, ref items) => {
                let mut result = String::new();

                let mut seen_methods = HashSet::new();
                for item in items.iter() {
                    let part = match *item {
                        MethodImplItem(ref method) => {
                            let base_name = match method.node {
                                MethDecl(ref ident, _, _, _, _, _, _, _) => ident.trans(trcx),
                                MethMac(_) => panic!("unexpected MethMac"),
                            };
                            seen_methods.insert(base_name);

                            let name = mangled_def_name(trcx, local_def(method.id));
                            try_str(|| trans_method(trcx, self, &**method), &*name)
                        },
                        TypeImplItem(ref td) => {
                            let name = mangled_def_name(trcx, local_def(td.id));
                            try_str(|| {
                                let name_str = td.ident.trans(trcx);
                                let self_str = self_ty.trans(trcx);
                                let typ_str = td.typ.trans(trcx);
                                format!("associated_type {} {} {}",
                                        impl_generics.trans_extra(trcx, TypeSpace),
                                        trans_impl_clause(trcx,
                                                          trait_ref.as_ref().unwrap(),
                                                          name_str,
                                                          self_str,
                                                          [].as_slice(),
                                                          [].as_slice()),
                                        typ_str)
                            }, &*name)
                        },
                    };
                    result.push_str(part.as_slice());
                    result.push_str("\n");
                }

                let opt_ty_trait_ref = ty::impl_trait_ref(trcx.tcx, local_def(self.id));
                if let Some(ty_trait_ref) = opt_ty_trait_ref {
                    for item in ty::trait_items(trcx.tcx, ty_trait_ref.def_id).iter() {
                        match *item {
                            ty::MethodTraitItem(ref method) => {
                                let base_name = method.name.trans(trcx);
                                if !seen_methods.contains(&base_name) {
                                    let self_ty_str = self_ty.trans(trcx);
                                    let method_name = method.name.trans(trcx);
                                    let i = trans_impl_clause(trcx,
                                                              trait_ref.as_ref().unwrap(),
                                                              method_name,
                                                              self_ty_str,
                                                              [].as_slice(),
                                                              [].as_slice());
                                    let code = format!("use_default {} {}",
                                                       impl_generics.trans_extra(trcx, TypeSpace),
                                                       i);
                                    /*let did = method.def_id;
                                    let name = mangled_def_name(trcx, did);
                                    trcx.observed_abstract_fns.insert(name, did);*/
                                    result.push_str(code.as_slice());
                                    result.push_str("\n");
                                }
                            },
                            ty::TypeTraitItem(..) => {},
                        }
                    }
                }

                result
            },
            ItemConst(ref ty, ref expr) => {
                format!("const {} {} {}",
                        mangled_def_name(trcx, local_def(self.id)),
                        ty.trans(trcx),
                        expr.trans(trcx))
            },
            ItemForeignMod(ref fm) => {
                let abi_str = format!("{:?}", fm.abi);
                let mut result = String::new();
                for item in fm.items.iter() {
                    let part = match item.node {
                        ForeignItemFn(ref decl, ref generics) => {
                            let name = mangled_def_name(trcx, local_def(item.id));
                            try_str(|| {
                                format!("extern_fn {} {} {} {}",
                                        abi_str,
                                        name,
                                        generics.trans_extra(trcx, FnSpace),
                                        decl.trans(trcx))
                            }, &*name)
                        },
                        ForeignItemStatic(ref ty, is_mutbl) => {
                            let name = mangled_def_name(trcx, local_def(item.id));
                            try_str(|| {
                                panic!("can't translate ForeignItemStatic");
                            }, &*name)
                        },
                    };
                    result.push_str(part.as_slice());
                    result.push_str("\n");
                }
                result

            },
            ItemStatic(ref ty, _, ref ex) => {
                let mangled_name = mangled_def_name(trcx, local_def(self.id));
                format!("static {} {} {}", mangled_name, ty.trans(trcx), ex.trans(trcx))
            },
            ItemTrait(unsafety, ref trait_generics, ref bounds, ref items) => {
                let mut result = String::new();
                for item in items.iter() {
                    let part = match *item {
                        RequiredMethod(ref method) => {
                            let did = local_def(method.id);
                            let name = mangled_def_name(trcx, did);
                            trcx.observed_abstract_fns.insert(name, did);
                            format!("# unimplemented trait method")
                        },
                        ProvidedMethod(ref method) => {
                            let did = local_def(method.id);
                            let name = mangled_def_name(trcx, did);
                            trcx.observed_abstract_fns.insert(name.clone(), did);
                            try_str(|| trans_method(trcx, self, &**method), &*name)
                        },
                        _ => format!("# non-method trait item"),
                    };
                    result.push_str(part.as_slice());
                    result.push_str("\n");
                }
                result
            },
            _ => format!(""),
        }
    }
}

fn combine_generics(trcx: &mut TransCtxt,
                    impl_g: &Generics,
                    fn_g: &Generics,
                    add_self: bool) -> (Vec<String>, Vec<String>) {
    let lifetimes =
            impl_g.lifetimes.iter().map(|l| format!("r_named_0_{}", l.lifetime.id)).chain(
            fn_g.lifetimes.iter().map(|l| format!("r_named_0_{}", l.lifetime.id))).collect();
    let ty_params =
            range(0, impl_g.ty_params.len()).map(|i| format!("t_{}", i)).chain(
            (if add_self { Some(String::from_str("s_0")) } else { None }).into_iter()).chain(
            range(0, fn_g.ty_params.len()).map(|i| format!("f_{}", i))).collect();
    (lifetimes, ty_params)
}

fn clean_path_elem(s: &str, out: &mut String) {
    let mut depth = 0us;
    for c in s.chars() {
        if c == '<' {
            depth += 1;
            out.push_str("$");
            out.push_str(depth.to_string().as_slice());
            out.push_str("$");
        } else if c == '>' {
            out.push_str("$");
            out.push_str(depth.to_string().as_slice());
            out.push_str("$");
            depth -= 1;
        } else {
            if c == '.' {
                out.push_str("$$");
            } else {
                out.push(c);
            }
        }
    }
}

fn mangled_def_name(trcx: &mut TransCtxt, did: DefId) -> String {
    let mut name = String::new();
    if did.krate == LOCAL_CRATE {
        name.push_str(&*trcx.crate_name);
        name.push_str("$");
        trcx.tcx.map.with_path(did.node, |mut elems| {
            for elem in elems {
                clean_path_elem(elem.name().as_str(), &mut name);
                name.push_str("$");
            }
        })
    } else {
        for elem in csearch::get_item_path(trcx.tcx, did).into_iter() {
            clean_path_elem(elem.name().as_str(), &mut name);
            name.push_str("$");
        }
    }
    name.pop();

    sanitize_ident(&*name)
}

fn sanitize_ident(s: &str) -> String {
    let mut last_i = 0;
    let mut result = String::with_capacity(s.len());
    for (i, c) in s.chars().enumerate() {
        if (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_' || c == '$' {
            continue;
        }
        result.push_str(s.slice(last_i, i));

        let n = c as u32;
        if n <= 0xff {
            result.push_str(&*format!("$x{:02x}", n));
        } else if n <= 0xffff {
            result.push_str(&*format!("$u{:04x}", n));
        } else {
            result.push_str(&*format!("$U{:08x}", n));
        }
        last_i = i + 1;
    }
    result.push_str(s.slice_from(last_i));
    result
}

/*
fn find_item_ast<'tcx>(tcx: &ty::ctxt<'tcx>, did: DefId) -> Option<&'tcx Item> {
    if did.krate == LOCAL_CRATE {
        match tcx.map.get(did.node) {
            ast_map::NodeItem(ast) => Some(ast),
            _ => panic!("expected NodeItem"),
        }
    } else {
        let result = csearch::maybe_get_item_ast(
            tcx, did,
            |a,b,c,d| astencode::decode_inlined_item(a, b, c, d));
        let item = match result {
            csearch::not_found => return None,
            csearch::found(item) => item,
            csearch::found_parent(_, item) => item,
        };
        match item {
            &IIItem(ref ast) => Some(&**ast),
            _ => panic!("expected IIItem"),
        }
    }
}

fn find_trait_item_ast<'tcx>(tcx: &ty::ctxt<'tcx>, did: DefId) -> Option<&'tcx TraitItem> {
    if did.krate == LOCAL_CRATE {
        match tcx.map.get(did.node) {
            ast_map::NodeTraitItem(ast) => Some(ast),
            _ => panic!("expected NodeTraitItem"),
        }
    } else {
        let result = csearch::maybe_get_item_ast(
            tcx, did,
            |a,b,c,d| astencode::decode_inlined_item(a, b, c, d));
        let item = match result {
            csearch::not_found => return None,
            csearch::found(item) => item,
            csearch::found_parent(_, item) => item,
        };
        match item {
            &IITraitItem(_, ref ast) => Some(ast),
            _ => panic!("expected IITraitItem"),
        }
    }
}
*/

fn trans_method(trcx: &mut TransCtxt, trait_: &Item, method: &Method) -> String {
    let mangled_name = mangled_def_name(trcx, local_def(method.id));

    let (is_default, impl_generics, trait_ref, self_ty_str) = match trait_.node {
        ItemImpl(_, _, ref generics, ref trait_ref, ref self_ty, _) =>
            (false, generics, trait_ref.as_ref(), self_ty.trans(trcx)),
        ItemTrait(_, ref generics, _, _) =>
            (true, generics, None, String::from_str("var s_0")),
        _ => panic!("expected ItemImpl"),
    };

    let (name, generics, _, exp_self, style, decl, body, vis) = match method.node {
        MethDecl(a, ref b, c, ref d, e, ref f, ref g, h) => (a, b, c, d, e, f, g, h),
        MethMac(_) => panic!("unexpected MethMac"),
    };

    let mut arg_strs = vec![];



    let self_arg = match exp_self.node {
        SelfStatic => None,
        SelfValue(ref name) => {
            Some(format!("({} var {})",
                         self_ty_str,
                         name.trans(trcx)))
        },
        SelfRegion(ref opt_lifetime, mutbl, ref name) => {
            Some(format!("({} {} {} var {})",
                         match mutbl {
                             MutMutable => "ref_mut",
                             MutImmutable => "ref",
                         },
                         match *opt_lifetime {
                             Some(ref lifetime) =>
                                 lifetime.trans(trcx),
                             None => format!("r_anon_0"),
                         },
                         self_ty_str,
                         name.trans(trcx)))
        },
        SelfExplicit(ref ty, ref name) => {
            Some(format!("({} var {})",
                         ty.trans(trcx),
                         name.trans(trcx)))
        },
    };
    let offset = match self_arg {
        Some(arg) => {
            arg_strs.push(arg);
            1
        },
        None => 0,
    };

    let impl_clause =
        if !is_default {
            match trait_ref {
                Some(trait_ref) => {
                    let name_str = name.trans(trcx);
                    format!("1 {}",
                            trans_impl_clause(trcx,
                                              trait_ref,
                                              name_str,
                                              self_ty_str,
                                              generics.lifetimes.as_slice(),
                                              generics.ty_params.as_slice()))
                },
                None => format!("0"),
            }
        } else {
            format!("0")
        };

    let (lifetimes, mut ty_params) = combine_generics(trcx, impl_generics, generics, is_default);
    let vis_str =
        if trait_ref.is_none() {
            vis.trans(trcx)
        } else {
            format!("pub")
        };

    arg_strs.extend(decl.inputs.slice_from(offset).iter().map(|x| x.trans(trcx)));
    format!("fn {} {}{} {} {} (args {}) return {} {} body {} {} {{\n{}\t{}\n}}\n\n",
            vis_str,
            mangled_name,
            if is_default { "$$__default" } else { "" },
            lifetimes.trans(trcx),
            ty_params.trans(trcx),
            arg_strs.trans(trcx),
            decl.output.trans(trcx),
            impl_clause,
            decl.output.trans(trcx),
            match style {
                Unsafety::Unsafe => "unsafe",
                Unsafety::Normal => "block",
            },
            body.stmts.trans(trcx),
            body.expr.as_ref().map(|e| e.trans(trcx))
                .unwrap_or(format!("[unit] simple_literal _method")))
}

fn add_fn_lifetimes(trcx: &mut TransCtxt,
                    fn_lifetimes: &[LifetimeDef],
                    lifetimes: &mut Vec<String>) {
    lifetimes.extend(fn_lifetimes.iter().map(|l| format!("r_named_0_{}", l.lifetime.id)));
}

fn add_fn_ty_params(trcx: &mut TransCtxt,
                    fn_ty_params: &[TyParam],
                    ty_params: &mut Vec<String>) {
    ty_params.extend(range(0, fn_ty_params.len()).map(|i| format!("var f_{}", i)));
}

fn trans_impl_clause(trcx: &mut TransCtxt,
                     trait_ref: &TraitRef,
                     name: String,
                     self_ty: String,
                     fn_lifetimes: &[LifetimeDef],
                     fn_ty_params: &[TyParam]) -> String {
    let last_seg = trait_ref.path.segments.as_slice().last().unwrap();
    let (mut lifetimes, mut ty_params): (Vec<_>, Vec<_>) = match last_seg.parameters {
        AngleBracketedParameters(ref params) => {
            let trait_refs = trcx.tcx.trait_refs.borrow();
            let substs = trait_refs[trait_ref.ref_id].substs;
            let ls = match substs.regions {
                subst::ErasedRegions => panic!("unsupported ErasedRegions"),
                subst::NonerasedRegions(ref regions) =>
                    regions.as_slice().iter().map(|r| r.trans(trcx)).collect(),
            };
            let ts = substs.types.as_slice().iter().map(|t| t.trans(trcx)).collect();
            (ls, ts)
        },
        ParenthesizedParameters(_) =>
            panic!("unsupported ParenthesizedParameters"),
    };

    add_fn_lifetimes(trcx, fn_lifetimes, &mut lifetimes);
    add_fn_ty_params(trcx, fn_ty_params, &mut ty_params);

    format!("{}${} {} {}",
            mangled_def_name(trcx, trcx.tcx.def_map.borrow()[trait_ref.ref_id].def_id()),
            name.trans(trcx),
            lifetimes.trans(trcx),
            ty_params.trans(trcx))
}

fn print_abstract_fn_decls(trcx: &mut TransCtxt) {
    let mut names = trcx.observed_abstract_fns.iter()
                        .map(|(k,v)| (k.clone(), v.clone()))
                        .collect::<Vec<_>>();
    names.sort();
    let names = names;


    for (name, method_did) in names.into_iter() {
        println!("{}", try_str(|| {
            let (method_generics, inputs, output) = {
                let trait_defs = trcx.tcx.trait_defs.borrow();
                let impl_or_trait_items = trcx.tcx.impl_or_trait_items.borrow();
                let opt_method = impl_or_trait_items.get(&method_did);

                let method = match opt_method {
                    None => panic!("can't find method for {}", name),
                    Some(x) => match x {
                        &ty::MethodTraitItem(ref m) => &**m,
                        _ => panic!("expected MethodTraitItem"),
                    }
                };

                (method.generics.clone(),
                 method.fty.sig.0.inputs.clone(),
                 method.fty.sig.0.output.clone())
            };

            let mut args = Vec::new();
            for (i, arg_ty) in inputs.iter().enumerate() {
                args.push(format!("({} var arg{})", arg_ty.trans(trcx), i));
            }

            let mut regions = Vec::new();
            for region in method_generics.regions.iter() {
                regions.push(format!("{}{}",
                                     region.space.trans(trcx),
                                     region.index));
            }

            let mut types = Vec::new();
            for ty_param in method_generics.types.iter() {
                types.push(format!("{}{}",
                                   ty_param.space.trans(trcx),
                                   ty_param.index));
            }

            let return_ty = match output {
                ty::FnConverging(ty) => ty.trans(trcx),
                ty::FnDiverging => format!("bottom"),
            };

            format!("abstract_fn {} {} {} args {} return {}",
                    name,
                    regions.trans(trcx),
                    types.trans(trcx),
                    args.trans(trcx),
                    return_ty)
        }, &*name));
    }
}

fn print_abstract_type_decls(trcx: &mut TransCtxt) {
    let mut names = trcx.observed_abstract_types.iter()
                        .map(|(k,v)| (k.clone(), v.clone()))
                        .collect::<Vec<_>>();
    names.sort();
    let names = names;

    for (name, trait_did) in names.into_iter() {
        println!("{}", try_str(|| {
            let trait_generics = {
                let trait_defs = trcx.tcx.trait_defs.borrow();
                let opt_trait = trait_defs.get(&trait_did);
                let trait_ = match opt_trait {
                    None => panic!("can't find trait for {}", name),
                    Some(ref t) => &**t,
                };

                trait_.generics.clone()
            };

            let mut regions = Vec::new();
            for region in trait_generics.regions.iter() {
                regions.push(format!("{}{}",
                                     region.space.trans(trcx),
                                     region.index));
            }

            let mut types = Vec::new();
            for ty_param in trait_generics.types.iter() {
                types.push(format!("{}{}",
                                   ty_param.space.trans(trcx),
                                   ty_param.index));
            }

            format!("abstract_type {} {} {}",
                    name,
                    regions.trans(trcx),
                    types.trans(trcx))
        }, &*name));
    }
}


pub fn process(tcx: &ty::ctxt, filter_fn : HashSet<String>, crate_name: String) {
    let krate = tcx.map.krate();
    let mut trcx = TransCtxt {
        tcx: tcx,
        observed_abstract_fns: HashMap::new(),
        observed_abstract_types: HashMap::new(),
        crate_name: crate_name,
    };
    {
        let mut visitor = TransVisitor { trcx: &mut trcx, filter_fn: filter_fn };
        visit::walk_crate(&mut visitor, krate);
    }
    print_abstract_fn_decls(&mut trcx);
    print_abstract_type_decls(&mut trcx);
}
