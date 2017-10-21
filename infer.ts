
type Expression = EInt | EVar | EFunc | ECall | EIf | ELet;
interface EInt
    { nodeType: "Int", value: number }
interface EVar
    { nodeType: "Var", name: string }
interface EFunc
    { nodeType: "Function", param: string, body: Expression }
interface ECall
    { nodeType: "Call", func: Expression, arg: Expression }
interface EIf {
    nodeType: "If",
    cond: Expression,
    trueBranch: Expression,
    falseBranch: Expression
}
interface ELet {
    nodeType: "Let",
    name: string;
    rhs: Expression;
    body: Expression;
}

type Type = TNamed | TVar | TFun;
interface TNamed { nodeType: "Named", name: string }
interface TVar { nodeType: "Var", name: string }
interface TFun { nodeType: "Function", from: Type, to: Type };
interface Forall {
    nodeType: "Forall",
    quantifiers: string[],
    type: Type;
};

type Context = {
    next: number;
    env: Env;
}
type Env = {
    [name: string]: Type | Forall;
};

type Substitution = {
    [key: string]: Type;
};

function typeToString(t: Type | Forall) {
    switch (t.nodeType) {
    case "Named":
    case "Var":
        return t.name;
    case "Function":
        return `(${typeToString(t.from)} -> ${typeToString(t.to)})`;
    case "Forall":
        return `(forall ${t.quantifiers.join(", ")}. ${typeToString(t.type)})`;
    }
}

function unify(t1: Type, t2: Type): Substitution {
    if (t1.nodeType === "Named"
        && t2.nodeType === "Named"
        && t2.name === t1.name) {
        return {};
    } else if (t1.nodeType === "Var") {
        return varBind(t1.name, t2);
    } else if (t2.nodeType === "Var") {
        return varBind(t2.name, t1);
    } else if (t1.nodeType === "Function" && t2.nodeType === "Function") {
        const s1 = unify(t1.from, t2.from);
        const s2 = unify(
            applySubstToType(s1, t1.to),
            applySubstToType(s1, t2.to)
        );
        return composeSubst(s1, s2);
    } else {
        throw `Type mismatch:\n    Expected ${typeToString(t1)}\n    Found ${typeToString(t2)}`;
    }
}

function varBind(name: string, t: Type): Substitution {
    if (t.nodeType === "Var" && t.name === name)  {
        return {};
    } else if (contains(t, name)) {
        throw `Type ${typeToString(t)} contains a reference to itself`;
    } else {
        const subst: Substitution = {};
        subst[name] = t;
        return subst;
    }
}

function contains(t: Type, name: string): boolean {
    switch (t.nodeType) {
    case "Named": return false;
    case "Var": return t.name === name;
    case "Function": return contains(t.from, name) || contains(t.to, name);
    }
}

function composeSubst(s1: Substitution, s2: Substitution): Substitution {
    const result: Substitution = {};
    for (const k in s2) {
        const type = s2[k];
        result[k] = applySubstToType(s1, type);
    };
    return { ...s1, ...result };
}

function applySubstToType(subst: Substitution, type: Type): Type {
    switch (type.nodeType) {
    case "Named": return type;
    case "Var":
        if (subst[type.name]) {
            return subst[type.name];
        } else {
            return type;
        }
    case "Function":
        return {
            nodeType: "Function",
            from: applySubstToType(subst, type.from),
            to: applySubstToType(subst, type.to)
        };
    }
}

function applySubstToForall(subst: Substitution, type: Forall): Forall {
    const substWithoutBound = { ...subst };
    for (const name in subst) {
        if (type.quantifiers.indexOf(name) > -1) {
            delete substWithoutBound[name];
        }
    }
    return {
        ...type,
        type: applySubstToType(substWithoutBound, type.type)
    }
}

function addToContext(ctx: Context, name: string, type: Type | Forall): Context {
    const newEnv = {
        ...ctx,
        env: { ...ctx.env }
    };
    newEnv.env[name] = type;
    return newEnv;
}

function newTVar(ctx: Context): TVar {
    const newVarNum = ctx.next;
    ctx.next++;
    return {
        nodeType: "Var",
        name: `T${newVarNum}`
    };
}

function infer(ctx: Context, e: Expression): [Type, Substitution] {
    const env = ctx.env;
    switch (e.nodeType) {
    case "Int": return [{ nodeType: "Named", name: "Int" }, {}];
    case "Var":
        return inferVar(ctx, e);
    case "If": return inferIf(ctx, e);
    case "Function": return inferFunction(ctx, e);
    case "Call":
        return inferCall(ctx, e);
    case "Let": return inferLet(ctx, e);
    }
}

function inferVar(ctx: Context, e: EVar): [Type, Substitution] {
    const env = ctx.env;
    if (env[e.name]) {
        const envType = env[e.name];
        if (envType.nodeType === "Forall") {
            return [instantiate(ctx, envType), {}];
        } else {
            return [envType, {}]
        }
    } else {
        throw `Unbound var ${e.name}`;
    }
}

function inferLet(ctx: Context, expr: ELet): [Type, Substitution] {
    const [rhsType, s1] = infer(ctx, expr.rhs);
    const ctx1 = applySubstToCtx(s1, ctx);
    const rhsPolytype = generalize(ctx1.env, rhsType);
    const ctx2 = addToContext(ctx1, expr.name, rhsPolytype);
    const [bodyType, s2] = infer(ctx2, expr.body);
    const s3 = { ...s1, ...s2 };
    return [bodyType, s3]
}

function generalize(env: Env, type: Type): Type | Forall {
    const envFreeVars = freeTypeVarsInEnv(env);
    const typeFreeVars = freeTypeVarsInType(type);
    const quantifiers = Object.keys(difference(typeFreeVars, envFreeVars));
    if (quantifiers.length > 0) {
        return {
            nodeType: "Forall",
            quantifiers: quantifiers,
            type: type
        };
    } else {
        return type;
    }
}

function instantiate(ctx: Context, forall: Forall): Type {
    const subst: Substitution = {};
    for (const name of forall.quantifiers) {
        const tVar = newTVar(ctx);
        subst[name] = tVar;
    }
    return applySubstToType(subst, forall.type);
}

type FreeVars = {
    [name: string]: true;
}

function union(a: FreeVars, b: FreeVars): FreeVars {
    return { ...a, ...b };
}

function difference(a: FreeVars, b: FreeVars): FreeVars {
    const result = { ...a };
    for (const name in b) {
        if (result[name]) {
            delete result[name];
        }
    }
    return result;
}

function freeTypeVarsInType(t: Type): FreeVars {
    switch (t.nodeType) {
    case "Named": return {};
    case "Var": return {[t.name]: true};
    case "Function":
        return union(
            freeTypeVarsInType(t.from),
            freeTypeVarsInType(t.to)
        );
    }
}

function freeTypeVarsInEnv(env: Env): FreeVars {
    let result: FreeVars = {};
    for (const key in env) {
        const t = env[key];
        const freeVars = t.nodeType === "Forall"
            ? freeTypeVarsInForall(t)
            : freeTypeVarsInType(t);
        result = union(result, freeVars);
    }
    return result;
}

function freeTypeVarsInForall(t: Forall): FreeVars {
    const quantifiers: FreeVars = {};
    for (const name of t.quantifiers) {
        quantifiers[name] = true;
    }
    const freeInType = freeTypeVarsInType(t.type);
    return difference(freeInType, quantifiers);
}

function inferFunction(ctx: Context, e: EFunc): [Type, Substitution] {
    const newType = newTVar(ctx);
    const newCtx = addToContext(ctx, e.param, newType);
    const [bodyType, subst] = infer(newCtx, e.body);
    const inferredType: Type = {
        nodeType: "Function",
        from: applySubstToType(subst, newType),
        to: bodyType
    };
    return [inferredType, subst];
}

function inferIf(ctx: Context, e: EIf): [Type, Substitution] {
    const [condType, s0] = infer(ctx, e.cond);
    const s1 = unify(condType, {
        nodeType: "Named",
        name: "Bool"
    });
    const ctx1 = applySubstToCtx(composeSubst(s0, s1), ctx);
    const [_trueBranchType, s2] = infer(ctx1, e.trueBranch);
    const s3 = composeSubst(s1, s2);
    const ctx2 = applySubstToCtx(s2, ctx1);
    const [_falseBranchType, s4] = infer(ctx2, e.falseBranch);
    const s5 = composeSubst(s3, s4);

    const trueBranchType = applySubstToType(s5, _trueBranchType);
    const falseBranchType = applySubstToType(s5, _falseBranchType);
    const s6 = unify(trueBranchType, falseBranchType);
    const resultSubst = composeSubst(s5, s6);
    return [
        applySubstToType(s6, trueBranchType),
        s6
    ];
}

function inferCall(ctx: Context, e: ECall): [Type, Substitution] {
    const [funcType, s1] = infer(ctx, e.func);
    const [argType, s2] = infer(applySubstToCtx(s1, ctx), e.arg);
    const newVar = newTVar(ctx);
    const s3 = composeSubst(s1, s2);
    const s4 = unify({
        nodeType: "Function",
        from: argType,
        to: newVar
    }, funcType);
    const s5 = composeSubst(s3, s4);
    const s6 = unify(applySubstToType(s5, (funcType as TFun).from), argType);
    const resultSubst = composeSubst(s5, s6);
    return [applySubstToType(resultSubst, (funcType as TFun).to), resultSubst];
}

function applySubstToCtx(subst: Substitution, ctx: Context): Context {
    const newContext = {
        ...ctx,
        env: {
            ...ctx.env
        }
    };
    for (const name in newContext.env) {
        const t = newContext.env[name];
        newContext.env[name] = t.nodeType === "Forall"
            ? applySubstToForall(subst, t)
            : applySubstToType(subst, t);
    }
    return newContext;
}
const initialEnv = {
    "true": tn("Bool"),
    "false": tn("Bool"),
    "!": tfunc(tn("Bool"), tn("Bool")),
    "&&": tfunc(tn("Bool"), tn("Bool"), tn("Bool")),
    "||": tfunc(tn("Bool"), tn("Bool"), tn("Bool")),
    "==": tfunc(tv("A"), tv("A"), tv("Bool")),
    "+": tfunc(tn("Int"), tn("Int"), tn("Int"))
}

console.log(
    typeToString(
        infer(
            {
                next: 0,
                env: initialEnv
            },
            eLet(
                "id", f("x", "x"),
                c("==", c("id", i(1)), c("id", "true"))
            )
        )[0]
    )
)

function v(name: string): Expression {
    return {
        nodeType: "Var",
        name: name
    };
}

function i(value: number): Expression {
    return {
        nodeType: "Int",
        value: value
    };
}

function f(param: string, body: Expression | string): Expression {
    return {
        nodeType: "Function",
        param: param,
        body: typeof body === "string" ? v(body) : body
    };
}

function c(f: Expression | string, ..._args: (Expression | string)[]): Expression {
    const args = _args.map(a => typeof a === "string" ? v(a) : a);
    return args.reduce(
        (func, arg) => ({
            nodeType: "Call",
            func: typeof func === "string" ? v(func) : func,
            arg: typeof arg === "string" ? v(arg) : arg
        }),
        typeof f === "string" ? v(f) : f
    );
}

function e(expr: Expression | string): Expression {
    if (typeof expr === "string") {
        return v(expr);
    } else {
        return expr;
    }
}

function eIf(
    _cond: Expression | string,
    _trueBranch: Expression | string,
    _falseBranch: Expression | string
): Expression {
    const cond = e(_cond);
    const trueBranch = e(_trueBranch);
    const falseBranch = e(_falseBranch);
    return {
        nodeType: "If",
        cond, trueBranch, falseBranch
    }
}

function eLet(
    name: string,
    _rhs: string | Expression,
    _body: string | Expression
): Expression {
    const rhs = e(_rhs),
          body = e(_body);
    return {
          nodeType: "Let",
          name, rhs, body
    }
}

function tn(name: string): Type {
    return {
        nodeType: "Named",
        name: name
    };
}
function tv(name: string): Type {
    return {
        nodeType: "Var",
        name: name
    };
}
function tfunc(...types: Type[]): Type {
    return types.reduceRight((to, from) => ({
        nodeType: "Function",
        from: from,
        to: to
    }));
}
