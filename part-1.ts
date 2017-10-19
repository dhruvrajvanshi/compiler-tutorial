
type Expression
    = { nodeType: "Int", value: number }
    | { nodeType: "Var", name: string }
    | { nodeType: "Function", param: string, body: Expression }
    | { nodeType: "Call", func: Expression, arg: Expression }
    | { nodeType: "If"
      , cond: Expression
      , trueBranch: Expression
      , falseBranch: Expression };

type Type
    = { nodeType: "Named", name: string }
    | { nodeType: "Var", name: string }
    | { nodeType: "Function", from: Type, to: Type };

type Context = {
    next: number;
    env: Env;
}
type Env = {
    [name: string]: Type;
};

type Substitution = {
    [key: string]: Type;
};

function typeToString(t: Type) {
    switch (t.nodeType) {
    case "Named":
    case "Var":
        return t.name;
    case "Function":
        return `(${typeToString(t.from)} -> ${typeToString(t.to)})`;
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
        const s2 = unify(t1.to, t2.to);
        return Object.assign({}, s1, s2);
    } else {
        throw `Type mismatch between ${typeToString(t1)} and ${typeToString(t2)}`;
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

function addToContext(ctx: Context, name: string, type: Type): Context {
    const newEnv = {
        ...ctx,
        env: { ...ctx.env }
    };
    newEnv.env[name] = type;
    return newEnv;
}

function newTVar(ctx: Context): Type {
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
        if (env[e.name]) {
            return [env[e.name], {}]
        } else {
            throw `Unbound var ${e.name}`;
        }
    case "If": {
        const [condType, s0] = infer(ctx, e.cond);
        const s1 = unify(condType, {
            nodeType: "Named",
            name: "Bool"
        });
        const ctx1 = applySubstToCtx(Object.assign({}, s0, s1), ctx);
        const [_trueBranchType, s2] = infer(ctx1, e.trueBranch);
        const s3 = Object.assign({}, s1, s2);
        const ctx2 = applySubstToCtx(s2, ctx1);
        const [_falseBranchType, s4] = infer(ctx2, e.falseBranch);
        const s5 = Object.assign({}, s3, s4);

        const trueBranchType = applySubstToType(s5, _trueBranchType);
        const falseBranchType = applySubstToType(s5, _falseBranchType);
        const s6 = unify(trueBranchType, falseBranchType);
        const resultSubst = Object.assign({}, s5, s6);
        return [
            applySubstToType(s6, trueBranchType),
            s6
        ];
    }
    case "Function":
        {
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
    case "Call":
        {
            const [funcType, s1] = infer(ctx, e.func);
            if (funcType.nodeType !== "Function") {
                throw `Expected a function; Got ${typeToString(funcType)}`;
            }
            const [argType, s2] = infer(applySubstToCtx(s1, ctx), e.arg);
            const resultSubst = Object.assign({}, unify(funcType.from, argType), s1, s2);
            return [applySubstToType(resultSubst, funcType.to), resultSubst];
        }
    }
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
        newContext.env[name] = applySubstToType(subst, t);
    }
    return newContext;
}

console.log(
    typeToString(infer({
        next: 0,
        env: {
            "true": tn("Bool"),
            "false": tn("Bool"),
            "!": tfunc(tn("Bool"), tn("Bool")),
            "&&": tfunc(tn("Bool"), tn("Bool"), tn("Bool")),
            "||": tfunc(tn("Bool"), tn("Bool"), tn("Bool")),
            "Int==": tfunc(tv("Int"), tv("Int"), tv("Bool")),
            "Bool==": tfunc(tv("Bool"), tv("Bool"), tv("Bool")),
            "+": tfunc(tn("Int"), tn("Int"), tn("Int"))
        }
    },
        c("&&",
          c("Int==", i(2), i(3)),
          c("Bool==", "true", "false")
        ),
    )[0]
))

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
