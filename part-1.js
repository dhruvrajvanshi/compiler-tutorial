;
;
function typeToString(t) {
    switch (t.nodeType) {
        case "Named":
        case "Var":
            return t.name;
        case "Function":
            return `(${typeToString(t.from)} -> ${typeToString(t.to)})`;
    }
}
function unify(t1, t2) {
    if (t1.nodeType === "Named"
        && t2.nodeType === "Named"
        && t2.name === t1.name) {
        return {};
    }
    else if (t1.nodeType === "Var") {
        return varBind(t1.name, t2);
    }
    else if (t2.nodeType === "Var") {
        return varBind(t2.name, t1);
    }
    else if (t1.nodeType === "Function" && t2.nodeType === "Function") {
        const s1 = unify(t1.from, t2.from);
        const s2 = unify(t1.to, t2.to);
        return Object.assign({}, s1, s2);
    }
    else {
        throw `Type mismatch:\n    Expected ${typeToString(t1)}\n    Found ${typeToString(t2)}`;
    }
}
function varBind(name, t) {
    if (t.nodeType === "Var" && t.name === name) {
        return {};
    }
    else if (contains(t, name)) {
        throw `Type ${typeToString(t)} contains a reference to itself`;
    }
    else {
        const subst = {};
        subst[name] = t;
        return subst;
    }
}
function contains(t, name) {
    switch (t.nodeType) {
        case "Named": return false;
        case "Var": return t.name === name;
        case "Function": return contains(t.from, name) || contains(t.to, name);
    }
}
function applySubstToType(subst, type) {
    switch (type.nodeType) {
        case "Named": return type;
        case "Var":
            if (subst[type.name]) {
                return subst[type.name];
            }
            else {
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
function addToContext(ctx, name, type) {
    const newEnv = Object.assign({}, ctx, { env: Object.assign({}, ctx.env) });
    newEnv.env[name] = type;
    return newEnv;
}
function newTVar(ctx) {
    const newVarNum = ctx.next;
    ctx.next++;
    return {
        nodeType: "Var",
        name: `T${newVarNum}`
    };
}
function infer(ctx, e) {
    const env = ctx.env;
    switch (e.nodeType) {
        case "Int": return [{ nodeType: "Named", name: "Int" }, {}];
        case "Var":
            if (env[e.name]) {
                return [env[e.name], {}];
            }
            else {
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
                const inferredType = {
                    nodeType: "Function",
                    from: applySubstToType(subst, newType),
                    to: bodyType
                };
                return [inferredType, subst];
            }
        case "Call":
            {
                const [funcType, s1] = infer(ctx, e.func);
                const [argType, s2] = infer(applySubstToCtx(s1, ctx), e.arg);
                const newVar = newTVar(ctx);
                const s3 = unify({
                    nodeType: "Function",
                    from: argType,
                    to: newVar
                }, funcType);
                const s4 = Object.assign({}, s1, s2, s3);
                const s5 = unify(applySubstToType(s4, funcType.from), argType);
                const resultSubst = Object.assign({}, s4, s5);
                return [applySubstToType(resultSubst, funcType.to), resultSubst];
            }
    }
}
function applySubstToCtx(subst, ctx) {
    const newContext = Object.assign({}, ctx, { env: Object.assign({}, ctx.env) });
    for (const name in newContext.env) {
        const t = newContext.env[name];
        newContext.env[name] = applySubstToType(subst, t);
    }
    return newContext;
}
console.log(typeToString(infer({
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
}, c("&&", c("true", i(2), i(3)), c("Bool==", "true", "false")))[0]));
function v(name) {
    return {
        nodeType: "Var",
        name: name
    };
}
function i(value) {
    return {
        nodeType: "Int",
        value: value
    };
}
function f(param, body) {
    return {
        nodeType: "Function",
        param: param,
        body: typeof body === "string" ? v(body) : body
    };
}
function c(f, ..._args) {
    const args = _args.map(a => typeof a === "string" ? v(a) : a);
    return args.reduce((func, arg) => ({
        nodeType: "Call",
        func: typeof func === "string" ? v(func) : func,
        arg: typeof arg === "string" ? v(arg) : arg
    }), typeof f === "string" ? v(f) : f);
}
function tn(name) {
    return {
        nodeType: "Named",
        name: name
    };
}
function tv(name) {
    return {
        nodeType: "Var",
        name: name
    };
}
function tfunc(...types) {
    return types.reduceRight((to, from) => ({
        nodeType: "Function",
        from: from,
        to: to
    }));
}
