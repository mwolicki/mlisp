/******/ (function(modules) { // webpackBootstrap
/******/ 	// The module cache
/******/ 	var installedModules = {};
/******/
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/
/******/ 		// Check if module is in cache
/******/ 		if(installedModules[moduleId]) {
/******/ 			return installedModules[moduleId].exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = installedModules[moduleId] = {
/******/ 			i: moduleId,
/******/ 			l: false,
/******/ 			exports: {}
/******/ 		};
/******/
/******/ 		// Execute the module function
/******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/
/******/ 		// Flag the module as loaded
/******/ 		module.l = true;
/******/
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/
/******/
/******/ 	// expose the modules object (__webpack_modules__)
/******/ 	__webpack_require__.m = modules;
/******/
/******/ 	// expose the module cache
/******/ 	__webpack_require__.c = installedModules;
/******/
/******/ 	// identity function for calling harmony imports with the correct context
/******/ 	__webpack_require__.i = function(value) { return value; };
/******/
/******/ 	// define getter function for harmony exports
/******/ 	__webpack_require__.d = function(exports, name, getter) {
/******/ 		if(!__webpack_require__.o(exports, name)) {
/******/ 			Object.defineProperty(exports, name, {
/******/ 				configurable: false,
/******/ 				enumerable: true,
/******/ 				get: getter
/******/ 			});
/******/ 		}
/******/ 	};
/******/
/******/ 	// getDefaultExport function for compatibility with non-harmony modules
/******/ 	__webpack_require__.n = function(module) {
/******/ 		var getter = module && module.__esModule ?
/******/ 			function getDefault() { return module['default']; } :
/******/ 			function getModuleExports() { return module; };
/******/ 		__webpack_require__.d(getter, 'a', getter);
/******/ 		return getter;
/******/ 	};
/******/
/******/ 	// Object.prototype.hasOwnProperty.call
/******/ 	__webpack_require__.o = function(object, property) { return Object.prototype.hasOwnProperty.call(object, property); };
/******/
/******/ 	// __webpack_public_path__
/******/ 	__webpack_require__.p = "";
/******/
/******/ 	// Load entry module and return exports
/******/ 	return __webpack_require__(__webpack_require__.s = 150);
/******/ })
/************************************************************************/
/******/ ([
/* 0 */
/***/ (function(module, exports) {

var core = module.exports = {version: '2.4.0'};
if(typeof __e == 'number')__e = core; // eslint-disable-line no-undef

/***/ }),
/* 1 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* unused harmony export NonDeclaredType */
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "h", function() { return Any; });
/* unused harmony export Unit */
/* unused harmony export Option */
/* unused harmony export Array */
/* harmony export (immutable) */ __webpack_exports__["k"] = Tuple;
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "m", function() { return FableFunction; });
/* harmony export (immutable) */ __webpack_exports__["l"] = GenericParam;
/* unused harmony export Interface */
/* harmony export (immutable) */ __webpack_exports__["e"] = makeGeneric;
/* unused harmony export isGeneric */
/* unused harmony export getDefinition */
/* unused harmony export extendInfo */
/* unused harmony export hasInterface */
/* unused harmony export getPropertyNames */
/* unused harmony export isArray */
/* harmony export (immutable) */ __webpack_exports__["a"] = toString;
/* unused harmony export hash */
/* harmony export (immutable) */ __webpack_exports__["c"] = equals;
/* unused harmony export comparePrimitives */
/* harmony export (immutable) */ __webpack_exports__["b"] = compare;
/* harmony export (immutable) */ __webpack_exports__["i"] = equalsRecords;
/* harmony export (immutable) */ __webpack_exports__["j"] = compareRecords;
/* harmony export (immutable) */ __webpack_exports__["g"] = equalsUnions;
/* harmony export (immutable) */ __webpack_exports__["d"] = compareUnions;
/* unused harmony export createDisposable */
/* unused harmony export createAtom */
/* unused harmony export createObj */
/* unused harmony export toPlainJsObj */
/* unused harmony export round */
/* unused harmony export randomNext */
/* harmony export (immutable) */ __webpack_exports__["f"] = defaultArg;
/* unused harmony export applyOperator */
/* unused harmony export parseNumber */
/* unused harmony export tryParse */
/* unused harmony export parse */
/* unused harmony export unescapeDataString */
/* unused harmony export escapeDataString */
/* unused harmony export escapeUriString */
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_object_get_own_property_descriptor__ = __webpack_require__(91);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_object_get_own_property_descriptor___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_object_get_own_property_descriptor__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_map__ = __webpack_require__(61);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_map___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_map__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_defineProperty__ = __webpack_require__(67);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_defineProperty___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_defineProperty__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_get_iterator__ = __webpack_require__(14);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_get_iterator___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_get_iterator__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_array_from__ = __webpack_require__(19);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_array_from___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_array_from__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_5_babel_runtime_core_js_json_stringify__ = __webpack_require__(88);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_5_babel_runtime_core_js_json_stringify___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_5_babel_runtime_core_js_json_stringify__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_6_babel_runtime_core_js_symbol_iterator__ = __webpack_require__(23);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_6_babel_runtime_core_js_symbol_iterator___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_6_babel_runtime_core_js_symbol_iterator__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_7_babel_runtime_core_js_object_assign__ = __webpack_require__(89);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_7_babel_runtime_core_js_object_assign___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_7_babel_runtime_core_js_object_assign__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_8_babel_runtime_core_js_object_get_own_property_names__ = __webpack_require__(92);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_8_babel_runtime_core_js_object_get_own_property_names___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_8_babel_runtime_core_js_object_get_own_property_names__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_9_babel_runtime_core_js_object_get_prototype_of__ = __webpack_require__(64);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_9_babel_runtime_core_js_object_get_prototype_of___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_9_babel_runtime_core_js_object_get_prototype_of__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_10_babel_runtime_helpers_typeof__ = __webpack_require__(30);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_10_babel_runtime_helpers_typeof___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_10_babel_runtime_helpers_typeof__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_11_babel_runtime_helpers_classCallCheck__ = __webpack_require__(9);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_11_babel_runtime_helpers_classCallCheck___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_11_babel_runtime_helpers_classCallCheck__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_12_babel_runtime_helpers_createClass__ = __webpack_require__(10);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_12_babel_runtime_helpers_createClass___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_12_babel_runtime_helpers_createClass__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_13__Symbol__ = __webpack_require__(6);














var NonDeclaredType = function () {
    function NonDeclaredType(kind, definition, generics) {
        __WEBPACK_IMPORTED_MODULE_11_babel_runtime_helpers_classCallCheck___default()(this, NonDeclaredType);

        this.kind = kind;
        this.definition = definition;
        this.generics = generics;
    }

    __WEBPACK_IMPORTED_MODULE_12_babel_runtime_helpers_createClass___default()(NonDeclaredType, [{
        key: "Equals",
        value: function Equals(other) {
            if (this.kind === other.kind && this.definition === other.definition) {
                return __WEBPACK_IMPORTED_MODULE_10_babel_runtime_helpers_typeof___default()(this.generics) === "object" ? equalsRecords(this.generics, other.generics) : this.generics === other.generics;
            }
            return false;
        }
    }]);

    return NonDeclaredType;
}();
var Any = new NonDeclaredType("Any");
var Unit = new NonDeclaredType("Unit");
function Option(t) {
    return new NonDeclaredType("Option", null, [t]);
}
function FableArray(t) {
    var isTypedArray = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : false;

    var def = null;
    var genArg = null;
    if (isTypedArray) {
        def = t;
    } else {
        genArg = t;
    }
    return new NonDeclaredType("Array", def, [genArg]);
}

function Tuple(types) {
    return new NonDeclaredType("Tuple", null, types);
}
function FableFunction(types) {
    return new NonDeclaredType("Function", null, types);
}

function GenericParam(definition) {
    return new NonDeclaredType("GenericParam", definition);
}
function Interface(definition) {
    return new NonDeclaredType("Interface", definition);
}
function makeGeneric(typeDef, genArgs) {
    return new NonDeclaredType("GenericType", typeDef, genArgs);
}
function isGeneric(typ) {
    return typ instanceof NonDeclaredType && typ.kind === "GenericType";
}
/**
 * Returns the parent if this is a declared generic type or the argument otherwise.
 * Attention: Unlike .NET this doesn't throw an exception if type is not generic.
 */
function getDefinition(typ) {
    return isGeneric(typ) ? typ.definition : typ;
}
function extendInfo(cons, info) {
    var parent = __WEBPACK_IMPORTED_MODULE_9_babel_runtime_core_js_object_get_prototype_of___default()(cons.prototype);
    if (typeof parent[__WEBPACK_IMPORTED_MODULE_13__Symbol__["a" /* default */].reflection] === "function") {
        var newInfo = {};
        var parentInfo = parent[__WEBPACK_IMPORTED_MODULE_13__Symbol__["a" /* default */].reflection]();
        __WEBPACK_IMPORTED_MODULE_8_babel_runtime_core_js_object_get_own_property_names___default()(info).forEach(function (k) {
            var i = info[k];
            if ((typeof i === "undefined" ? "undefined" : __WEBPACK_IMPORTED_MODULE_10_babel_runtime_helpers_typeof___default()(i)) === "object") {
                newInfo[k] = Array.isArray(i) ? (parentInfo[k] || []).concat(i) : __WEBPACK_IMPORTED_MODULE_7_babel_runtime_core_js_object_assign___default()(parentInfo[k] || {}, i);
            } else {
                newInfo[k] = i;
            }
        });
        return newInfo;
    }
    return info;
}
function hasInterface(obj, interfaceName) {
    if (interfaceName === "System.Collections.Generic.IEnumerable") {
        return typeof obj[__WEBPACK_IMPORTED_MODULE_6_babel_runtime_core_js_symbol_iterator___default.a] === "function";
    } else if (typeof obj[__WEBPACK_IMPORTED_MODULE_13__Symbol__["a" /* default */].reflection] === "function") {
        var interfaces = obj[__WEBPACK_IMPORTED_MODULE_13__Symbol__["a" /* default */].reflection]().interfaces;
        return Array.isArray(interfaces) && interfaces.indexOf(interfaceName) > -1;
    }
    return false;
}
/**
 * Returns:
 * - Records: array with names of fields
 * - Classes: array with names of getters
 * - Nulls and unions: empty array
 * - JS Objects: The result of calling Object.getOwnPropertyNames
 */
function getPropertyNames(obj) {
    if (obj == null) {
        return [];
    }
    var propertyMap = typeof obj[__WEBPACK_IMPORTED_MODULE_13__Symbol__["a" /* default */].reflection] === "function" ? obj[__WEBPACK_IMPORTED_MODULE_13__Symbol__["a" /* default */].reflection]().properties || [] : obj;
    return __WEBPACK_IMPORTED_MODULE_8_babel_runtime_core_js_object_get_own_property_names___default()(propertyMap);
}
function isArray(obj) {
    return Array.isArray(obj) || ArrayBuffer.isView(obj);
}
function toString(obj) {
    var quoteStrings = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : false;

    function isObject(x) {
        return x !== null && (typeof x === "undefined" ? "undefined" : __WEBPACK_IMPORTED_MODULE_10_babel_runtime_helpers_typeof___default()(x)) === "object" && !(x instanceof Number) && !(x instanceof String) && !(x instanceof Boolean);
    }
    if (obj == null || typeof obj === "number") {
        return String(obj);
    }
    if (typeof obj === "string") {
        return quoteStrings ? __WEBPACK_IMPORTED_MODULE_5_babel_runtime_core_js_json_stringify___default()(obj) : obj;
    }
    if (typeof obj.ToString === "function") {
        return obj.ToString();
    }
    if (hasInterface(obj, "FSharpUnion")) {
        var info = obj[__WEBPACK_IMPORTED_MODULE_13__Symbol__["a" /* default */].reflection]();
        var uci = info.cases[obj.tag];
        switch (uci.length) {
            case 1:
                return uci[0];
            case 2:
                // For simplicity let's always use parens, in .NET they're ommitted in some cases
                return uci[0] + " (" + toString(obj.data, true) + ")";
            default:
                return uci[0] + " (" + obj.data.map(function (x) {
                    return toString(x, true);
                }).join(",") + ")";
        }
    }
    try {
        return __WEBPACK_IMPORTED_MODULE_5_babel_runtime_core_js_json_stringify___default()(obj, function (k, v) {
            return v && v[__WEBPACK_IMPORTED_MODULE_6_babel_runtime_core_js_symbol_iterator___default.a] && !Array.isArray(v) && isObject(v) ? __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_array_from___default()(v) : v && typeof v.ToString === "function" ? toString(v) : v;
        });
    } catch (err) {
        // Fallback for objects with circular references
        return "{" + __WEBPACK_IMPORTED_MODULE_8_babel_runtime_core_js_object_get_own_property_names___default()(obj).map(function (k) {
            return k + ": " + String(obj[k]);
        }).join(", ") + "}";
    }
}
function hash(x) {
    if (x != null && typeof x.GetHashCode === "function") {
        return x.GetHashCode();
    } else {
        var s = __WEBPACK_IMPORTED_MODULE_5_babel_runtime_core_js_json_stringify___default()(x);
        var h = 5381;
        var i = 0;
        var len = s.length;
        while (i < len) {
            h = h * 33 ^ s.charCodeAt(i++);
        }
        return h;
    }
}
function equals(x, y) {
    // Optimization if they are referencially equal
    if (x === y) {
        return true;
    } else if (x == null) {
        return y == null;
    } else if (y == null) {
        return false;
    } else if (__WEBPACK_IMPORTED_MODULE_9_babel_runtime_core_js_object_get_prototype_of___default()(x) !== __WEBPACK_IMPORTED_MODULE_9_babel_runtime_core_js_object_get_prototype_of___default()(y)) {
        return false;
        // Equals override or IEquatable implementation
    } else if (typeof x.Equals === "function") {
        return x.Equals(y);
    } else if (Array.isArray(x)) {
        if (x.length !== y.length) {
            return false;
        }
        for (var i = 0; i < x.length; i++) {
            if (!equals(x[i], y[i])) {
                return false;
            }
        }
        return true;
    } else if (ArrayBuffer.isView(x)) {
        if (x.byteLength !== y.byteLength) {
            return false;
        }
        var dv1 = new DataView(x.buffer);
        var dv2 = new DataView(y.buffer);
        for (var _i = 0; _i < x.byteLength; _i++) {
            if (dv1.getUint8(_i) !== dv2.getUint8(_i)) {
                return false;
            }
        }
        return true;
    } else if (x instanceof Date) {
        return x.getTime() === y.getTime();
    } else {
        return false;
    }
}
function comparePrimitives(x, y) {
    return x === y ? 0 : x < y ? -1 : 1;
}
function compare(x, y) {
    // Optimization if they are referencially equal
    if (x === y) {
        return 0;
    } else if (x == null) {
        return y == null ? 0 : -1;
    } else if (y == null) {
        return 1; // everything is bigger than null
    } else if (__WEBPACK_IMPORTED_MODULE_9_babel_runtime_core_js_object_get_prototype_of___default()(x) !== __WEBPACK_IMPORTED_MODULE_9_babel_runtime_core_js_object_get_prototype_of___default()(y)) {
        return -1;
        // Some types (see Long.ts) may just implement the function and not the interface
        // else if (hasInterface(x, "System.IComparable"))
    } else if (typeof x.CompareTo === "function") {
        return x.CompareTo(y);
    } else if (Array.isArray(x)) {
        if (x.length !== y.length) {
            return x.length < y.length ? -1 : 1;
        }
        for (var i = 0, j = 0; i < x.length; i++) {
            j = compare(x[i], y[i]);
            if (j !== 0) {
                return j;
            }
        }
        return 0;
    } else if (ArrayBuffer.isView(x)) {
        if (x.byteLength !== y.byteLength) {
            return x.byteLength < y.byteLength ? -1 : 1;
        }
        var dv1 = new DataView(x.buffer);
        var dv2 = new DataView(y.buffer);
        for (var _i2 = 0, b1 = 0, b2 = 0; _i2 < x.byteLength; _i2++) {
            b1 = dv1.getUint8(_i2), b2 = dv2.getUint8(_i2);
            if (b1 < b2) {
                return -1;
            }
            if (b1 > b2) {
                return 1;
            }
        }
        return 0;
    } else if (x instanceof Date) {
        var xtime = x.getTime();
        var ytime = y.getTime();
        return xtime === ytime ? 0 : xtime < ytime ? -1 : 1;
    } else if ((typeof x === "undefined" ? "undefined" : __WEBPACK_IMPORTED_MODULE_10_babel_runtime_helpers_typeof___default()(x)) === "object") {
        var xhash = hash(x);
        var yhash = hash(y);
        if (xhash === yhash) {
            return equals(x, y) ? 0 : -1;
        } else {
            return xhash < yhash ? -1 : 1;
        }
    } else {
        return x < y ? -1 : 1;
    }
}
function equalsRecords(x, y) {
    // Optimization if they are referencially equal
    if (x === y) {
        return true;
    } else {
        var keys = getPropertyNames(x);
        var _iteratorNormalCompletion = true;
        var _didIteratorError = false;
        var _iteratorError = undefined;

        try {
            for (var _iterator = __WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_get_iterator___default()(keys), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
                var key = _step.value;

                if (!equals(x[key], y[key])) {
                    return false;
                }
            }
        } catch (err) {
            _didIteratorError = true;
            _iteratorError = err;
        } finally {
            try {
                if (!_iteratorNormalCompletion && _iterator.return) {
                    _iterator.return();
                }
            } finally {
                if (_didIteratorError) {
                    throw _iteratorError;
                }
            }
        }

        return true;
    }
}
function compareRecords(x, y) {
    // Optimization if they are referencially equal
    if (x === y) {
        return 0;
    } else {
        var keys = getPropertyNames(x);
        var _iteratorNormalCompletion2 = true;
        var _didIteratorError2 = false;
        var _iteratorError2 = undefined;

        try {
            for (var _iterator2 = __WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_get_iterator___default()(keys), _step2; !(_iteratorNormalCompletion2 = (_step2 = _iterator2.next()).done); _iteratorNormalCompletion2 = true) {
                var key = _step2.value;

                var res = compare(x[key], y[key]);
                if (res !== 0) {
                    return res;
                }
            }
        } catch (err) {
            _didIteratorError2 = true;
            _iteratorError2 = err;
        } finally {
            try {
                if (!_iteratorNormalCompletion2 && _iterator2.return) {
                    _iterator2.return();
                }
            } finally {
                if (_didIteratorError2) {
                    throw _iteratorError2;
                }
            }
        }

        return 0;
    }
}
function equalsUnions(x, y) {
    return x === y || x.tag === y.tag && equals(x.data, y.data);
}
function compareUnions(x, y) {
    if (x === y) {
        return 0;
    } else {
        var res = x.tag < y.tag ? -1 : x.tag > y.tag ? 1 : 0;
        return res !== 0 ? res : compare(x.data, y.data);
    }
}
function createDisposable(f) {
    return __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_defineProperty___default()({
        Dispose: f
    }, __WEBPACK_IMPORTED_MODULE_13__Symbol__["a" /* default */].reflection, function () {
        return { interfaces: ["System.IDisposable"] };
    });
}
// tslint forbids non-arrow functions, but it's
// necessary here to use the arguments object
/* tslint:disable */
function createAtom(value) {
    var atom = value;
    return function () {
        return arguments.length === 0 ? atom : (atom = arguments[0], void 0);
    };
}
/* tslint:enable */
var CaseRules = {
    None: 0,
    LowerFirst: 1
};
function isList(o) {
    if (o != null) {
        if (typeof o[__WEBPACK_IMPORTED_MODULE_13__Symbol__["a" /* default */].reflection] === "function") {
            return o[__WEBPACK_IMPORTED_MODULE_13__Symbol__["a" /* default */].reflection]().type === "Microsoft.FSharp.Collections.FSharpList";
        }
    }
    return false;
}
function createObj(fields) {
    var caseRule = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : CaseRules.None;
    var casesCache = arguments[2];

    var iter = __WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_get_iterator___default()(fields);
    var cur = iter.next();
    var o = {};
    while (!cur.done) {
        var value = cur.value;
        if (Array.isArray(value)) {
            o[value[0]] = value[1];
        } else {
            casesCache = casesCache || new __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_map___default.a();
            var proto = __WEBPACK_IMPORTED_MODULE_9_babel_runtime_core_js_object_get_prototype_of___default()(value);
            var cases = casesCache.get(proto);
            if (cases == null) {
                if (typeof proto[__WEBPACK_IMPORTED_MODULE_13__Symbol__["a" /* default */].reflection] === "function") {
                    cases = proto[__WEBPACK_IMPORTED_MODULE_13__Symbol__["a" /* default */].reflection]().cases;
                    casesCache.set(proto, cases);
                }
            }
            var caseInfo = cases != null ? cases[value.tag] : null;
            if (Array.isArray(caseInfo)) {
                var key = caseInfo[0];
                if (caseRule === CaseRules.LowerFirst) {
                    key = key[0].toLowerCase() + key.substr(1);
                }
                o[key] = caseInfo.length === 1 ? true : isList(value.data) ? createObj(value.data, caseRule, casesCache) : value.data;
            } else {
                throw new Error("Cannot infer key and value of " + value);
            }
        }
        cur = iter.next();
    }
    return o;
}
function toPlainJsObj(source) {
    if (source != null && source.constructor !== Object) {
        var target = {};
        var props = __WEBPACK_IMPORTED_MODULE_8_babel_runtime_core_js_object_get_own_property_names___default()(source);
        var _iteratorNormalCompletion3 = true;
        var _didIteratorError3 = false;
        var _iteratorError3 = undefined;

        try {
            for (var _iterator3 = __WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_get_iterator___default()(props), _step3; !(_iteratorNormalCompletion3 = (_step3 = _iterator3.next()).done); _iteratorNormalCompletion3 = true) {
                var _p = _step3.value;

                target[_p] = source[_p];
            }
            // Copy also properties from prototype, see #192
        } catch (err) {
            _didIteratorError3 = true;
            _iteratorError3 = err;
        } finally {
            try {
                if (!_iteratorNormalCompletion3 && _iterator3.return) {
                    _iterator3.return();
                }
            } finally {
                if (_didIteratorError3) {
                    throw _iteratorError3;
                }
            }
        }

        var proto = __WEBPACK_IMPORTED_MODULE_9_babel_runtime_core_js_object_get_prototype_of___default()(source);
        if (proto != null) {
            props = __WEBPACK_IMPORTED_MODULE_8_babel_runtime_core_js_object_get_own_property_names___default()(proto);
            var _iteratorNormalCompletion4 = true;
            var _didIteratorError4 = false;
            var _iteratorError4 = undefined;

            try {
                for (var _iterator4 = __WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_get_iterator___default()(props), _step4; !(_iteratorNormalCompletion4 = (_step4 = _iterator4.next()).done); _iteratorNormalCompletion4 = true) {
                    var p = _step4.value;

                    var prop = __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_object_get_own_property_descriptor___default()(proto, p);
                    if (prop.value) {
                        target[p] = prop.value;
                    } else if (prop.get) {
                        target[p] = prop.get.apply(source);
                    }
                }
            } catch (err) {
                _didIteratorError4 = true;
                _iteratorError4 = err;
            } finally {
                try {
                    if (!_iteratorNormalCompletion4 && _iterator4.return) {
                        _iterator4.return();
                    }
                } finally {
                    if (_didIteratorError4) {
                        throw _iteratorError4;
                    }
                }
            }
        }
        return target;
    } else {
        return source;
    }
}
function round(value) {
    var digits = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 0;

    var m = Math.pow(10, digits);
    var n = +(digits ? value * m : value).toFixed(8);
    var i = Math.floor(n);
    var f = n - i;
    var e = 1e-8;
    var r = f > 0.5 - e && f < 0.5 + e ? i % 2 === 0 ? i : i + 1 : Math.round(n);
    return digits ? r / m : r;
}
function randomNext(min, max) {
    return Math.floor(Math.random() * (max - min)) + min;
}
function defaultArg(arg, defaultValue, f) {
    return arg == null ? defaultValue : f != null ? f(arg) : arg;
}
function applyOperator(x, y, operator) {
    function getMethod(obj) {
        if ((typeof obj === "undefined" ? "undefined" : __WEBPACK_IMPORTED_MODULE_10_babel_runtime_helpers_typeof___default()(obj)) === "object") {
            var cons = __WEBPACK_IMPORTED_MODULE_9_babel_runtime_core_js_object_get_prototype_of___default()(obj).constructor;
            if (typeof cons[operator] === "function") {
                return cons[operator];
            }
        }
        return null;
    }
    var meth = getMethod(x);
    if (meth != null) {
        return meth(x, y);
    }
    meth = getMethod(y);
    if (meth != null) {
        return meth(x, y);
    }
    switch (operator) {
        case "op_Addition":
            return x + y;
        case "op_Subtraction":
            return x - y;
        case "op_Multiply":
            return x * y;
        case "op_Division":
            return x / y;
        case "op_Modulus":
            return x % y;
        case "op_LeftShift":
            return x << y;
        case "op_RightShift":
            return x >> y;
        case "op_BitwiseAnd":
            return x & y;
        case "op_BitwiseOr":
            return x | y;
        case "op_ExclusiveOr":
            return x ^ y;
        case "op_LogicalNot":
            return x + y;
        case "op_UnaryNegation":
            return !x;
        case "op_BooleanAnd":
            return x && y;
        case "op_BooleanOr":
            return x || y;
        default:
            return null;
    }
}
function parseNumber(v) {
    return +v;
}
function tryParse(v, initial, parser, fn) {
    if (v != null) {
        var a = parser.exec(v);
        if (a !== null) {
            return [true, fn(a[1])];
        }
    }
    return [false, initial];
}
function parse(v, initial, parser, fn) {
    var a = tryParse(v, initial, parser, fn);
    if (a[0]) {
        return a[1];
    } else {
        // TODO FormatException ?
        throw new Error("Input string was not in a correct format.");
    }
}
function unescapeDataString(s) {
    // https://stackoverflow.com/a/4458580/524236
    return decodeURIComponent(s.replace(/\+/g, "%20"));
}
function escapeDataString(s) {
    return encodeURIComponent(s).replace(/!/g, "%21").replace(/'/g, "%27").replace(/\(/g, "%28").replace(/\)/g, "%29").replace(/\*/g, "%2A");
}
function escapeUriString(s) {
    return encodeURI(s);
}

/***/ }),
/* 2 */
/***/ (function(module, exports, __webpack_require__) {

var store      = __webpack_require__(53)('wks')
  , uid        = __webpack_require__(36)
  , Symbol     = __webpack_require__(8).Symbol
  , USE_SYMBOL = typeof Symbol == 'function';

var $exports = module.exports = function(name){
  return store[name] || (store[name] =
    USE_SYMBOL && Symbol[name] || (USE_SYMBOL ? Symbol : uid)('Symbol.' + name));
};

$exports.store = store;

/***/ }),
/* 3 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* unused harmony export Enumerator */
/* unused harmony export getEnumerator */
/* unused harmony export toIterator */
/* harmony export (immutable) */ __webpack_exports__["d"] = toList;
/* unused harmony export ofList */
/* unused harmony export ofArray */
/* unused harmony export append */
/* unused harmony export average */
/* unused harmony export averageBy */
/* unused harmony export concat */
/* unused harmony export collect */
/* unused harmony export choose */
/* harmony export (immutable) */ __webpack_exports__["f"] = compareWith;
/* unused harmony export delay */
/* unused harmony export empty */
/* unused harmony export enumerateWhile */
/* unused harmony export enumerateThenFinally */
/* unused harmony export enumerateUsing */
/* unused harmony export exactlyOne */
/* unused harmony export except */
/* unused harmony export exists */
/* unused harmony export exists2 */
/* unused harmony export filter */
/* unused harmony export where */
/* harmony export (immutable) */ __webpack_exports__["c"] = fold;
/* harmony export (immutable) */ __webpack_exports__["l"] = foldBack;
/* unused harmony export fold2 */
/* unused harmony export foldBack2 */
/* harmony export (immutable) */ __webpack_exports__["a"] = forAll;
/* unused harmony export forAll2 */
/* unused harmony export tryHead */
/* unused harmony export head */
/* unused harmony export initialize */
/* unused harmony export initializeInfinite */
/* unused harmony export tryItem */
/* unused harmony export item */
/* unused harmony export iterate */
/* unused harmony export iterate2 */
/* unused harmony export iterateIndexed */
/* unused harmony export iterateIndexed2 */
/* unused harmony export isEmpty */
/* unused harmony export tryLast */
/* unused harmony export last */
/* unused harmony export count */
/* harmony export (immutable) */ __webpack_exports__["g"] = map;
/* unused harmony export mapIndexed */
/* unused harmony export map2 */
/* unused harmony export mapIndexed2 */
/* unused harmony export map3 */
/* unused harmony export chunkBySize */
/* unused harmony export mapFold */
/* unused harmony export mapFoldBack */
/* unused harmony export max */
/* unused harmony export maxBy */
/* unused harmony export min */
/* unused harmony export minBy */
/* unused harmony export pairwise */
/* unused harmony export permute */
/* unused harmony export rangeStep */
/* harmony export (immutable) */ __webpack_exports__["j"] = rangeChar;
/* unused harmony export range */
/* unused harmony export readOnly */
/* harmony export (immutable) */ __webpack_exports__["b"] = reduce;
/* unused harmony export reduceBack */
/* unused harmony export replicate */
/* unused harmony export reverse */
/* unused harmony export scan */
/* unused harmony export scanBack */
/* unused harmony export singleton */
/* unused harmony export skip */
/* unused harmony export skipWhile */
/* unused harmony export sortWith */
/* unused harmony export sum */
/* unused harmony export sumBy */
/* unused harmony export tail */
/* unused harmony export take */
/* unused harmony export truncate */
/* unused harmony export takeWhile */
/* harmony export (immutable) */ __webpack_exports__["k"] = tryFind;
/* unused harmony export find */
/* unused harmony export tryFindBack */
/* unused harmony export findBack */
/* unused harmony export tryFindIndex */
/* unused harmony export findIndex */
/* unused harmony export tryFindIndexBack */
/* unused harmony export findIndexBack */
/* harmony export (immutable) */ __webpack_exports__["i"] = tryPick;
/* harmony export (immutable) */ __webpack_exports__["h"] = pick;
/* unused harmony export unfold */
/* harmony export (immutable) */ __webpack_exports__["e"] = zip;
/* unused harmony export zip3 */
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_slicedToArray__ = __webpack_require__(95);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_slicedToArray___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_slicedToArray__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from__ = __webpack_require__(19);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_defineProperty__ = __webpack_require__(67);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_defineProperty___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_defineProperty__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_symbol_iterator__ = __webpack_require__(23);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_symbol_iterator___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_symbol_iterator__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator__ = __webpack_require__(14);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_5_babel_runtime_helpers_classCallCheck__ = __webpack_require__(9);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_5_babel_runtime_helpers_classCallCheck___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_5_babel_runtime_helpers_classCallCheck__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_6_babel_runtime_helpers_createClass__ = __webpack_require__(10);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_6_babel_runtime_helpers_createClass___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_6_babel_runtime_helpers_createClass__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_7__Array__ = __webpack_require__(59);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_8__ListClass__ = __webpack_require__(22);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_9__Util__ = __webpack_require__(1);












var Enumerator = function () {
    function Enumerator(iter) {
        __WEBPACK_IMPORTED_MODULE_5_babel_runtime_helpers_classCallCheck___default()(this, Enumerator);

        this.iter = iter;
    }

    __WEBPACK_IMPORTED_MODULE_6_babel_runtime_helpers_createClass___default()(Enumerator, [{
        key: "MoveNext",
        value: function MoveNext() {
            var cur = this.iter.next();
            this.current = cur.value;
            return !cur.done;
        }
    }, {
        key: "Reset",
        value: function Reset() {
            throw new Error("JS iterators cannot be reset");
        }
    }, {
        key: "Dispose",
        value: function Dispose() {
            return;
        }
    }, {
        key: "Current",
        get: function get() {
            return this.current;
        }
    }]);

    return Enumerator;
}();
function getEnumerator(o) {
    return typeof o.GetEnumerator === "function" ? o.GetEnumerator() : new Enumerator(__WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(o));
}
function toIterator(en) {
    return {
        next: function next() {
            return en.MoveNext() ? { done: false, value: en.Current } : { done: true, value: null };
        }
    };
}
function __failIfNone(res) {
    if (res == null) {
        throw new Error("Seq did not contain any matching element");
    }
    return res;
}
function toList(xs) {
    return foldBack(function (x, acc) {
        return new __WEBPACK_IMPORTED_MODULE_8__ListClass__["b" /* default */](x, acc);
    }, xs, new __WEBPACK_IMPORTED_MODULE_8__ListClass__["b" /* default */]());
}
function ofList(xs) {
    return delay(function () {
        return unfold(function (x) {
            return x.tail != null ? [x.head, x.tail] : null;
        }, xs);
    });
}
function ofArray(xs) {
    return delay(function () {
        return unfold(function (i) {
            return i < xs.length ? [xs[i], i + 1] : null;
        }, 0);
    });
}
function append(xs, ys) {
    return delay(function () {
        var firstDone = false;
        var i = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
        var iters = [i, null];
        return unfold(function () {
            var cur = void 0;
            if (!firstDone) {
                cur = iters[0].next();
                if (!cur.done) {
                    return [cur.value, iters];
                } else {
                    firstDone = true;
                    iters = [null, __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(ys)];
                }
            }
            cur = iters[1].next();
            return !cur.done ? [cur.value, iters] : null;
        }, iters);
    });
}
function average(xs) {
    var count = 1;
    var sum = reduce(function (acc, x) {
        count++;
        return acc + x;
    }, xs);
    return sum / count;
}
function averageBy(f, xs) {
    var count = 1;
    var sum = reduce(function (acc, x) {
        count++;
        return (count === 2 ? f(acc) : acc) + f(x);
    }, xs);
    return sum / count;
}
function concat(xs) {
    return delay(function () {
        var iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
        var output = { value: null };
        return unfold(function (innerIter) {
            var hasFinished = false;
            while (!hasFinished) {
                if (innerIter == null) {
                    var cur = iter.next();
                    if (!cur.done) {
                        innerIter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(cur.value);
                    } else {
                        hasFinished = true;
                    }
                } else {
                    var _cur = innerIter.next();
                    if (!_cur.done) {
                        output = { value: _cur.value };
                        hasFinished = true;
                    } else {
                        innerIter = null;
                    }
                }
            }
            return innerIter != null && output != null ? [output.value, innerIter] : null;
        }, null);
    });
}
function collect(f, xs) {
    return concat(map(f, xs));
}
function choose(f, xs) {
    var trySkipToNext = function trySkipToNext(iter) {
        var cur = iter.next();
        if (!cur.done) {
            var y = f(cur.value);
            return y != null ? [y, iter] : trySkipToNext(iter);
        }
        return void 0;
    };
    return delay(function () {
        return unfold(function (iter) {
            return trySkipToNext(iter);
        }, __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs));
    });
}
function compareWith(f, xs, ys) {
    var nonZero = tryFind(function (i) {
        return i !== 0;
    }, map2(function (x, y) {
        return f(x, y);
    }, xs, ys));
    return nonZero != null ? nonZero : count(xs) - count(ys);
}
function delay(f) {
    return __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_defineProperty___default()({}, __WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_symbol_iterator___default.a, function () {
        return __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(f());
    });
}
function empty() {
    return unfold(function () {
        return void 0;
    });
}
function enumerateWhile(cond, xs) {
    return concat(unfold(function () {
        return cond() ? [xs, true] : null;
    }));
}
function enumerateThenFinally(xs, finalFn) {
    return delay(function () {
        var iter = void 0;
        try {
            iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
        } catch (err) {
            return void 0;
        } finally {
            finalFn();
        }
        return unfold(function (it) {
            try {
                var cur = it.next();
                return !cur.done ? [cur.value, it] : null;
            } catch (err) {
                return void 0;
            } finally {
                finalFn();
            }
        }, iter);
    });
}
function enumerateUsing(disp, work) {
    var isDisposed = false;
    var disposeOnce = function disposeOnce() {
        if (!isDisposed) {
            isDisposed = true;
            disp.Dispose();
        }
    };
    try {
        return enumerateThenFinally(work(disp), disposeOnce);
    } catch (err) {
        return void 0;
    } finally {
        disposeOnce();
    }
}
function exactlyOne(xs) {
    var iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
    var fst = iter.next();
    if (fst.done) {
        throw new Error("Seq was empty");
    }
    var snd = iter.next();
    if (!snd.done) {
        throw new Error("Seq had multiple items");
    }
    return fst.value;
}
function except(itemsToExclude, source) {
    var exclusionItems = __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default()(itemsToExclude);
    var testIsNotInExclusionItems = function testIsNotInExclusionItems(element) {
        return !exclusionItems.some(function (excludedItem) {
            return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__Util__["c" /* equals */])(excludedItem, element);
        });
    };
    return filter(testIsNotInExclusionItems, source);
}
function exists(f, xs) {
    function aux(iter) {
        var cur = iter.next();
        return !cur.done && (f(cur.value) || aux(iter));
    }
    return aux(__WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs));
}
function exists2(f, xs, ys) {
    function aux(iter1, iter2) {
        var cur1 = iter1.next();
        var cur2 = iter2.next();
        return !cur1.done && !cur2.done && (f(cur1.value, cur2.value) || aux(iter1, iter2));
    }
    return aux(__WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs), __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(ys));
}
function filter(f, xs) {
    function trySkipToNext(iter) {
        var cur = iter.next();
        while (!cur.done) {
            if (f(cur.value)) {
                return [cur.value, iter];
            }
            cur = iter.next();
        }
        return void 0;
    }
    return delay(function () {
        return unfold(trySkipToNext, __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs));
    });
}
function where(f, xs) {
    return filter(f, xs);
}
function fold(f, acc, xs) {
    if (Array.isArray(xs) || ArrayBuffer.isView(xs)) {
        return xs.reduce(f, acc);
    } else {
        var cur = void 0;
        for (var i = 0, iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);; i++) {
            cur = iter.next();
            if (cur.done) {
                break;
            }
            acc = f(acc, cur.value, i);
        }
        return acc;
    }
}
function foldBack(f, xs, acc) {
    var arr = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs : __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default()(xs);
    for (var i = arr.length - 1; i >= 0; i--) {
        acc = f(arr[i], acc, i);
    }
    return acc;
}
function fold2(f, acc, xs, ys) {
    var iter1 = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
    var iter2 = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(ys);
    var cur1 = void 0;
    var cur2 = void 0;
    for (var i = 0;; i++) {
        cur1 = iter1.next();
        cur2 = iter2.next();
        if (cur1.done || cur2.done) {
            break;
        }
        acc = f(acc, cur1.value, cur2.value, i);
    }
    return acc;
}
function foldBack2(f, xs, ys, acc) {
    var ar1 = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs : __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default()(xs);
    var ar2 = Array.isArray(ys) || ArrayBuffer.isView(ys) ? ys : __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default()(ys);
    for (var i = ar1.length - 1; i >= 0; i--) {
        acc = f(ar1[i], ar2[i], acc, i);
    }
    return acc;
}
function forAll(f, xs) {
    return fold(function (acc, x) {
        return acc && f(x);
    }, true, xs);
}
function forAll2(f, xs, ys) {
    return fold2(function (acc, x, y) {
        return acc && f(x, y);
    }, true, xs, ys);
}
function tryHead(xs) {
    var iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
    var cur = iter.next();
    return cur.done ? null : cur.value;
}
function head(xs) {
    return __failIfNone(tryHead(xs));
}
function initialize(n, f) {
    return delay(function () {
        return unfold(function (i) {
            return i < n ? [f(i), i + 1] : null;
        }, 0);
    });
}
function initializeInfinite(f) {
    return delay(function () {
        return unfold(function (i) {
            return [f(i), i + 1];
        }, 0);
    });
}
function tryItem(i, xs) {
    if (i < 0) {
        return null;
    }
    if (Array.isArray(xs) || ArrayBuffer.isView(xs)) {
        return i < xs.length ? xs[i] : null;
    }
    for (var j = 0, iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);; j++) {
        var cur = iter.next();
        if (cur.done) {
            return null;
        }
        if (j === i) {
            return cur.value;
        }
    }
}
function item(i, xs) {
    return __failIfNone(tryItem(i, xs));
}
function iterate(f, xs) {
    fold(function (_, x) {
        return f(x);
    }, null, xs);
}
function iterate2(f, xs, ys) {
    fold2(function (_, x, y) {
        return f(x, y);
    }, null, xs, ys);
}
function iterateIndexed(f, xs) {
    fold(function (_, x, i) {
        return f(i, x);
    }, null, xs);
}
function iterateIndexed2(f, xs, ys) {
    fold2(function (_, x, y, i) {
        return f(i, x, y);
    }, null, xs, ys);
}
function isEmpty(xs) {
    var i = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
    return i.next().done;
}
function tryLast(xs) {
    try {
        return reduce(function (_, x) {
            return x;
        }, xs);
    } catch (err) {
        return null;
    }
}
function last(xs) {
    return __failIfNone(tryLast(xs));
}
// A export function 'length' method causes problems in JavaScript -- https://github.com/Microsoft/TypeScript/issues/442
function count(xs) {
    return Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs.length : fold(function (acc, x) {
        return acc + 1;
    }, 0, xs);
}
function map(f, xs) {
    return delay(function () {
        return unfold(function (iter) {
            var cur = iter.next();
            return !cur.done ? [f(cur.value), iter] : null;
        }, __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs));
    });
}
function mapIndexed(f, xs) {
    return delay(function () {
        var i = 0;
        return unfold(function (iter) {
            var cur = iter.next();
            return !cur.done ? [f(i++, cur.value), iter] : null;
        }, __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs));
    });
}
function map2(f, xs, ys) {
    return delay(function () {
        var iter1 = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
        var iter2 = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(ys);
        return unfold(function () {
            var cur1 = iter1.next();
            var cur2 = iter2.next();
            return !cur1.done && !cur2.done ? [f(cur1.value, cur2.value), null] : null;
        });
    });
}
function mapIndexed2(f, xs, ys) {
    return delay(function () {
        var i = 0;
        var iter1 = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
        var iter2 = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(ys);
        return unfold(function () {
            var cur1 = iter1.next();
            var cur2 = iter2.next();
            return !cur1.done && !cur2.done ? [f(i++, cur1.value, cur2.value), null] : null;
        });
    });
}
function map3(f, xs, ys, zs) {
    return delay(function () {
        var iter1 = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
        var iter2 = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(ys);
        var iter3 = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(zs);
        return unfold(function () {
            var cur1 = iter1.next();
            var cur2 = iter2.next();
            var cur3 = iter3.next();
            return !cur1.done && !cur2.done && !cur3.done ? [f(cur1.value, cur2.value, cur3.value), null] : null;
        });
    });
}
function chunkBySize(size, xs) {
    var result = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__Array__["a" /* chunkBySize */])(size, __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default()(xs));
    return ofArray(result.map(ofArray));
}
function mapFold(f, acc, xs, transform) {
    var result = [];
    var r = void 0;
    var cur = void 0;
    for (var i = 0, iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);; i++) {
        cur = iter.next();
        if (cur.done) {
            break;
        }

        var _f = f(acc, cur.value);

        var _f2 = __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_slicedToArray___default()(_f, 2);

        r = _f2[0];
        acc = _f2[1];

        result.push(r);
    }
    return transform !== void 0 ? [transform(result), acc] : [result, acc];
}
function mapFoldBack(f, xs, acc, transform) {
    var arr = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs : __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default()(xs);
    var result = [];
    var r = void 0;
    for (var i = arr.length - 1; i >= 0; i--) {
        var _f3 = f(arr[i], acc);

        var _f4 = __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_slicedToArray___default()(_f3, 2);

        r = _f4[0];
        acc = _f4[1];

        result.push(r);
    }
    return transform !== void 0 ? [transform(result), acc] : [result, acc];
}
function max(xs) {
    return reduce(function (acc, x) {
        return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__Util__["b" /* compare */])(acc, x) === 1 ? acc : x;
    }, xs);
}
function maxBy(f, xs) {
    return reduce(function (acc, x) {
        return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__Util__["b" /* compare */])(f(acc), f(x)) === 1 ? acc : x;
    }, xs);
}
function min(xs) {
    return reduce(function (acc, x) {
        return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__Util__["b" /* compare */])(acc, x) === -1 ? acc : x;
    }, xs);
}
function minBy(f, xs) {
    return reduce(function (acc, x) {
        return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__Util__["b" /* compare */])(f(acc), f(x)) === -1 ? acc : x;
    }, xs);
}
function pairwise(xs) {
    return skip(2, scan(function (last, next) {
        return [last[1], next];
    }, [0, 0], xs));
}
function permute(f, xs) {
    return ofArray(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__Array__["b" /* permute */])(f, __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default()(xs)));
}
function rangeStep(first, step, last) {
    if (step === 0) {
        throw new Error("Step cannot be 0");
    }
    return delay(function () {
        return unfold(function (x) {
            return step > 0 && x <= last || step < 0 && x >= last ? [x, x + step] : null;
        }, first);
    });
}
function rangeChar(first, last) {
    return delay(function () {
        return unfold(function (x) {
            return x <= last ? [x, String.fromCharCode(x.charCodeAt(0) + 1)] : null;
        }, first);
    });
}
function range(first, last) {
    return rangeStep(first, 1, last);
}
function readOnly(xs) {
    return map(function (x) {
        return x;
    }, xs);
}
function reduce(f, xs) {
    if (Array.isArray(xs) || ArrayBuffer.isView(xs)) {
        return xs.reduce(f);
    }
    var iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
    var cur = iter.next();
    if (cur.done) {
        throw new Error("Seq was empty");
    }
    var acc = cur.value;
    while (true) {
        cur = iter.next();
        if (cur.done) {
            break;
        }
        acc = f(acc, cur.value);
    }
    return acc;
}
function reduceBack(f, xs) {
    var ar = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs : __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default()(xs);
    if (ar.length === 0) {
        throw new Error("Seq was empty");
    }
    var acc = ar[ar.length - 1];
    for (var i = ar.length - 2; i >= 0; i--) {
        acc = f(ar[i], acc, i);
    }
    return acc;
}
function replicate(n, x) {
    return initialize(n, function () {
        return x;
    });
}
function reverse(xs) {
    var ar = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs.slice(0) : __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default()(xs);
    return ofArray(ar.reverse());
}
function scan(f, seed, xs) {
    return delay(function () {
        var iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
        return unfold(function (acc) {
            if (acc == null) {
                return [seed, seed];
            }
            var cur = iter.next();
            if (!cur.done) {
                acc = f(acc, cur.value);
                return [acc, acc];
            }
            return void 0;
        }, null);
    });
}
function scanBack(f, xs, seed) {
    return reverse(scan(function (acc, x) {
        return f(x, acc);
    }, seed, reverse(xs)));
}
function singleton(y) {
    return unfold(function (x) {
        return x != null ? [x, null] : null;
    }, y);
}
function skip(n, xs) {
    return __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_defineProperty___default()({}, __WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_symbol_iterator___default.a, function () {
        var iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
        for (var i = 1; i <= n; i++) {
            if (iter.next().done) {
                throw new Error("Seq has not enough elements");
            }
        }
        return iter;
    });
}
function skipWhile(f, xs) {
    return delay(function () {
        var hasPassed = false;
        return filter(function (x) {
            return hasPassed || (hasPassed = !f(x));
        }, xs);
    });
}
function sortWith(f, xs) {
    var ys = __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default()(xs);
    return ofArray(ys.sort(f));
}
function sum(xs) {
    return fold(function (acc, x) {
        return acc + x;
    }, 0, xs);
}
function sumBy(f, xs) {
    return fold(function (acc, x) {
        return acc + f(x);
    }, 0, xs);
}
function tail(xs) {
    var iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
    var cur = iter.next();
    if (cur.done) {
        throw new Error("Seq was empty");
    }
    return __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_defineProperty___default()({}, __WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_symbol_iterator___default.a, function () {
        return iter;
    });
}
function take(n, xs) {
    var truncate = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : false;

    return delay(function () {
        var iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
        return unfold(function (i) {
            if (i < n) {
                var cur = iter.next();
                if (!cur.done) {
                    return [cur.value, i + 1];
                }
                if (!truncate) {
                    throw new Error("Seq has not enough elements");
                }
            }
            return void 0;
        }, 0);
    });
}
function truncate(n, xs) {
    return take(n, xs, true);
}
function takeWhile(f, xs) {
    return delay(function () {
        var iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
        return unfold(function (i) {
            var cur = iter.next();
            if (!cur.done && f(cur.value)) {
                return [cur.value, null];
            }
            return void 0;
        }, 0);
    });
}
function tryFind(f, xs, defaultValue) {
    for (var i = 0, iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);; i++) {
        var cur = iter.next();
        if (cur.done) {
            return defaultValue === void 0 ? null : defaultValue;
        }
        if (f(cur.value, i)) {
            return cur.value;
        }
    }
}
function find(f, xs) {
    return __failIfNone(tryFind(f, xs));
}
function tryFindBack(f, xs, defaultValue) {
    var match = null;
    for (var i = 0, iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);; i++) {
        var cur = iter.next();
        if (cur.done) {
            return match === null ? defaultValue === void 0 ? null : defaultValue : match;
        }
        if (f(cur.value, i)) {
            match = cur.value;
        }
    }
}
function findBack(f, xs) {
    return __failIfNone(tryFindBack(f, xs));
}
function tryFindIndex(f, xs) {
    for (var i = 0, iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);; i++) {
        var cur = iter.next();
        if (cur.done) {
            return null;
        }
        if (f(cur.value, i)) {
            return i;
        }
    }
}
function findIndex(f, xs) {
    return __failIfNone(tryFindIndex(f, xs));
}
function tryFindIndexBack(f, xs) {
    var match = -1;
    for (var i = 0, iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);; i++) {
        var cur = iter.next();
        if (cur.done) {
            return match === -1 ? null : match;
        }
        if (f(cur.value, i)) {
            match = i;
        }
    }
}
function findIndexBack(f, xs) {
    return __failIfNone(tryFindIndexBack(f, xs));
}
function tryPick(f, xs) {
    for (var i = 0, iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);; i++) {
        var cur = iter.next();
        if (cur.done) {
            break;
        }
        var y = f(cur.value, i);
        if (y != null) {
            return y;
        }
    }
    return void 0;
}
function pick(f, xs) {
    return __failIfNone(tryPick(f, xs));
}
function unfold(f, acc) {
    return __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_defineProperty___default()({}, __WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_symbol_iterator___default.a, function () {
        return {
            next: function next() {
                var res = f(acc);
                if (res != null) {
                    acc = res[1];
                    return { done: false, value: res[0] };
                }
                return { done: true };
            }
        };
    });
}
function zip(xs, ys) {
    return map2(function (x, y) {
        return [x, y];
    }, xs, ys);
}
function zip3(xs, ys, zs) {
    return map3(function (x, y, z) {
        return [x, y, z];
    }, xs, ys, zs);
}

/***/ }),
/* 4 */
/***/ (function(module, exports, __webpack_require__) {

var global    = __webpack_require__(8)
  , core      = __webpack_require__(0)
  , ctx       = __webpack_require__(20)
  , hide      = __webpack_require__(12)
  , PROTOTYPE = 'prototype';

var $export = function(type, name, source){
  var IS_FORCED = type & $export.F
    , IS_GLOBAL = type & $export.G
    , IS_STATIC = type & $export.S
    , IS_PROTO  = type & $export.P
    , IS_BIND   = type & $export.B
    , IS_WRAP   = type & $export.W
    , exports   = IS_GLOBAL ? core : core[name] || (core[name] = {})
    , expProto  = exports[PROTOTYPE]
    , target    = IS_GLOBAL ? global : IS_STATIC ? global[name] : (global[name] || {})[PROTOTYPE]
    , key, own, out;
  if(IS_GLOBAL)source = name;
  for(key in source){
    // contains in native
    own = !IS_FORCED && target && target[key] !== undefined;
    if(own && key in exports)continue;
    // export native or passed
    out = own ? target[key] : source[key];
    // prevent global pollution for namespaces
    exports[key] = IS_GLOBAL && typeof target[key] != 'function' ? source[key]
    // bind timers to global for call from export context
    : IS_BIND && own ? ctx(out, global)
    // wrap global constructors for prevent change them in library
    : IS_WRAP && target[key] == out ? (function(C){
      var F = function(a, b, c){
        if(this instanceof C){
          switch(arguments.length){
            case 0: return new C;
            case 1: return new C(a);
            case 2: return new C(a, b);
          } return new C(a, b, c);
        } return C.apply(this, arguments);
      };
      F[PROTOTYPE] = C[PROTOTYPE];
      return F;
    // make static versions for prototype methods
    })(out) : IS_PROTO && typeof out == 'function' ? ctx(Function.call, out) : out;
    // export proto methods to core.%CONSTRUCTOR%.methods.%NAME%
    if(IS_PROTO){
      (exports.virtual || (exports.virtual = {}))[key] = out;
      // export proto methods to core.%CONSTRUCTOR%.prototype.%NAME%
      if(type & $export.R && expProto && !expProto[key])hide(expProto, key, out);
    }
  }
};
// type bitmap
$export.F = 1;   // forced
$export.G = 2;   // global
$export.S = 4;   // static
$export.P = 8;   // proto
$export.B = 16;  // bind
$export.W = 32;  // wrap
$export.U = 64;  // safe
$export.R = 128; // real proto method for `library` 
module.exports = $export;

/***/ }),
/* 5 */
/***/ (function(module, exports, __webpack_require__) {

var anObject       = __webpack_require__(11)
  , IE8_DOM_DEFINE = __webpack_require__(70)
  , toPrimitive    = __webpack_require__(55)
  , dP             = Object.defineProperty;

exports.f = __webpack_require__(7) ? Object.defineProperty : function defineProperty(O, P, Attributes){
  anObject(O);
  P = toPrimitive(P, true);
  anObject(Attributes);
  if(IE8_DOM_DEFINE)try {
    return dP(O, P, Attributes);
  } catch(e){ /* empty */ }
  if('get' in Attributes || 'set' in Attributes)throw TypeError('Accessors not supported!');
  if('value' in Attributes)O[P] = Attributes.value;
  return O;
};

/***/ }),
/* 6 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony export (immutable) */ __webpack_exports__["b"] = setType;
/* unused harmony export getType */
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_symbol__ = __webpack_require__(66);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_symbol___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_symbol__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_map__ = __webpack_require__(61);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_map___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_map__);


var types = new __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_map___default.a();
function setType(fullName, cons) {
    types.set(fullName, cons);
}
function getType(fullName) {
    return types.get(fullName);
}
/* harmony default export */ __webpack_exports__["a"] = ({
    reflection: __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_symbol___default()("reflection")
});

/***/ }),
/* 7 */
/***/ (function(module, exports, __webpack_require__) {

// Thank's IE8 for his funny defineProperty
module.exports = !__webpack_require__(15)(function(){
  return Object.defineProperty({}, 'a', {get: function(){ return 7; }}).a != 7;
});

/***/ }),
/* 8 */
/***/ (function(module, exports) {

// https://github.com/zloirock/core-js/issues/86#issuecomment-115759028
var global = module.exports = typeof window != 'undefined' && window.Math == Math
  ? window : typeof self != 'undefined' && self.Math == Math ? self : Function('return this')();
if(typeof __g == 'number')__g = global; // eslint-disable-line no-undef

/***/ }),
/* 9 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


exports.__esModule = true;

exports.default = function (instance, Constructor) {
  if (!(instance instanceof Constructor)) {
    throw new TypeError("Cannot call a class as a function");
  }
};

/***/ }),
/* 10 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


exports.__esModule = true;

var _defineProperty = __webpack_require__(63);

var _defineProperty2 = _interopRequireDefault(_defineProperty);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

exports.default = function () {
  function defineProperties(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor) descriptor.writable = true;
      (0, _defineProperty2.default)(target, descriptor.key, descriptor);
    }
  }

  return function (Constructor, protoProps, staticProps) {
    if (protoProps) defineProperties(Constructor.prototype, protoProps);
    if (staticProps) defineProperties(Constructor, staticProps);
    return Constructor;
  };
}();

/***/ }),
/* 11 */
/***/ (function(module, exports, __webpack_require__) {

var isObject = __webpack_require__(17);
module.exports = function(it){
  if(!isObject(it))throw TypeError(it + ' is not an object!');
  return it;
};

/***/ }),
/* 12 */
/***/ (function(module, exports, __webpack_require__) {

var dP         = __webpack_require__(5)
  , createDesc = __webpack_require__(25);
module.exports = __webpack_require__(7) ? function(object, key, value){
  return dP.f(object, key, createDesc(1, value));
} : function(object, key, value){
  object[key] = value;
  return object;
};

/***/ }),
/* 13 */
/***/ (function(module, exports, __webpack_require__) {

// to indexed object, toObject with fallback for non-array-like ES3 strings
var IObject = __webpack_require__(45)
  , defined = __webpack_require__(31);
module.exports = function(it){
  return IObject(defined(it));
};

/***/ }),
/* 14 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(97), __esModule: true };

/***/ }),
/* 15 */
/***/ (function(module, exports) {

module.exports = function(exec){
  try {
    return !!exec();
  } catch(e){
    return true;
  }
};

/***/ }),
/* 16 */
/***/ (function(module, exports) {

var hasOwnProperty = {}.hasOwnProperty;
module.exports = function(it, key){
  return hasOwnProperty.call(it, key);
};

/***/ }),
/* 17 */
/***/ (function(module, exports) {

module.exports = function(it){
  return typeof it === 'object' ? it !== null : typeof it === 'function';
};

/***/ }),
/* 18 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* unused harmony export minValue */
/* unused harmony export maxValue */
/* unused harmony export parse */
/* unused harmony export tryParse */
/* unused harmony export create */
/* unused harmony export now */
/* unused harmony export utcNow */
/* unused harmony export today */
/* unused harmony export isLeapYear */
/* unused harmony export daysInMonth */
/* unused harmony export toUniversalTime */
/* unused harmony export toLocalTime */
/* unused harmony export timeOfDay */
/* unused harmony export date */
/* unused harmony export kind */
/* harmony export (immutable) */ __webpack_exports__["e"] = day;
/* harmony export (immutable) */ __webpack_exports__["c"] = hour;
/* unused harmony export millisecond */
/* harmony export (immutable) */ __webpack_exports__["f"] = minute;
/* harmony export (immutable) */ __webpack_exports__["d"] = month;
/* harmony export (immutable) */ __webpack_exports__["a"] = second;
/* harmony export (immutable) */ __webpack_exports__["b"] = year;
/* unused harmony export dayOfWeek */
/* unused harmony export ticks */
/* unused harmony export toBinary */
/* unused harmony export dayOfYear */
/* unused harmony export add */
/* unused harmony export addDays */
/* unused harmony export addHours */
/* unused harmony export addMinutes */
/* unused harmony export addSeconds */
/* unused harmony export addMilliseconds */
/* unused harmony export addTicks */
/* unused harmony export addYears */
/* unused harmony export addMonths */
/* unused harmony export subtract */
/* unused harmony export toLongDateString */
/* unused harmony export toShortDateString */
/* unused harmony export toLongTimeString */
/* unused harmony export toShortTimeString */
/* unused harmony export equals */
/* unused harmony export compare */
/* unused harmony export compareTo */
/* unused harmony export op_Addition */
/* unused harmony export op_Subtraction */
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0__Long__ = __webpack_require__(39);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1__TimeSpan__ = __webpack_require__(86);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2__Util__ = __webpack_require__(1);



function minValue() {
    return parse(-8640000000000000, 1);
}
function maxValue() {
    return parse(8640000000000000, 1);
}
/* tslint:disable */
function parse(v, kind) {
    if (kind == null) {
        kind = typeof v === "string" && v.slice(-1) === "Z" ? 1 /* UTC */ : 2 /* Local */;
    }
    var date = v == null ? new Date() : new Date(v);
    if (isNaN(date.getTime())) {
        // Check if this is a time-only string, which JS Date parsing cannot handle (see #1045)
        if (typeof v === "string" && /^(?:[01]?\d|2[0-3]):(?:[0-5]?\d)(?::[0-5]?\d(?:\.\d+)?)?(?:\s*[AaPp][Mm])?$/.test(v)) {
            var d = new Date();
            date = new Date(d.getFullYear() + "/" + (d.getMonth() + 1) + "/" + d.getDate() + " " + v);
        } else {
            throw new Error("The string is not a valid Date.");
        }
    }
    if (kind === 2 /* Local */) {
            date.kind = kind;
        }
    return date;
}
/* tslint:enable */
function tryParse(v) {
    try {
        return [true, parse(v)];
    } catch (_err) {
        return [false, minValue()];
    }
}
function create(year, month, day) /* Local */{
    var h = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : 0;
    var m = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : 0;
    var s = arguments.length > 5 && arguments[5] !== undefined ? arguments[5] : 0;
    var ms = arguments.length > 6 && arguments[6] !== undefined ? arguments[6] : 0;
    var kind = arguments.length > 7 && arguments[7] !== undefined ? arguments[7] : 2;

    var date = void 0;
    if (kind === 2 /* Local */) {
            date = new Date(year, month - 1, day, h, m, s, ms);
            date.kind = kind;
        } else {
        date = new Date(Date.UTC(year, month - 1, day, h, m, s, ms));
    }
    if (isNaN(date.getTime())) {
        throw new Error("The parameters describe an unrepresentable Date.");
    }
    return date;
}
function now() {
    return parse();
}
function utcNow() {
    return parse(null, 1);
}
function today() {
    return date(now());
}
function isLeapYear(year) {
    return year % 4 === 0 && year % 100 !== 0 || year % 400 === 0;
}
function daysInMonth(year, month) {
    return month === 2 ? isLeapYear(year) ? 29 : 28 : month >= 8 ? month % 2 === 0 ? 31 : 30 : month % 2 === 0 ? 30 : 31;
}
function toUniversalTime(d) {
    return d.kind === 2 /* Local */ ? new Date(d.getTime()) : d;
}
function toLocalTime(d) {
    if (d.kind === 2 /* Local */) {
            return d;
        } else {
        var d2 = new Date(d.getTime());
        d2.kind = 2 /* Local */;
        return d2;
    }
}
function timeOfDay(d) {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_1__TimeSpan__["a" /* create */])(0, hour(d), minute(d), second(d), millisecond(d));
}
function date(d) {
    return create(year(d), month(d), day(d), 0, 0, 0, 0, d.kind || 1 /* UTC */);
}
function kind(d) {
    return d.kind || 1 /* UTC */;
}
function day(d) {
    return d.kind === 2 /* Local */ ? d.getDate() : d.getUTCDate();
}
function hour(d) {
    return d.kind === 2 /* Local */ ? d.getHours() : d.getUTCHours();
}
function millisecond(d) {
    return d.kind === 2 /* Local */ ? d.getMilliseconds() : d.getUTCMilliseconds();
}
function minute(d) {
    return d.kind === 2 /* Local */ ? d.getMinutes() : d.getUTCMinutes();
}
function month(d) {
    return (d.kind === 2 /* Local */ ? d.getMonth() : d.getUTCMonth()) + 1;
}
function second(d) {
    return d.kind === 2 /* Local */ ? d.getSeconds() : d.getUTCSeconds();
}
function year(d) {
    return d.kind === 2 /* Local */ ? d.getFullYear() : d.getUTCFullYear();
}
function dayOfWeek(d) {
    return d.kind === 2 /* Local */ ? d.getDay() : d.getUTCDay();
}
function ticks(d) {
    return __WEBPACK_IMPORTED_MODULE_0__Long__["a" /* fromNumber */](d.getTime()).add(62135596800000) // UnixEpochMilliseconds
    .sub(d.kind === 2 /* Local */ ? d.getTimezoneOffset() * 60 * 1000 : 0).mul(10000);
}
function toBinary(d) {
    return ticks(d);
}
function dayOfYear(d) {
    var _year = year(d);
    var _month = month(d);
    var _day = day(d);
    for (var i = 1; i < _month; i++) {
        _day += daysInMonth(_year, i);
    }
    return _day;
}
function add(d, ts) {
    return parse(d.getTime() + ts, d.kind || 1 /* UTC */);
}
function addDays(d, v) {
    return parse(d.getTime() + v * 86400000, d.kind || 1 /* UTC */);
}
function addHours(d, v) {
    return parse(d.getTime() + v * 3600000, d.kind || 1 /* UTC */);
}
function addMinutes(d, v) {
    return parse(d.getTime() + v * 60000, d.kind || 1 /* UTC */);
}
function addSeconds(d, v) {
    return parse(d.getTime() + v * 1000, d.kind || 1 /* UTC */);
}
function addMilliseconds(d, v) {
    return parse(d.getTime() + v, d.kind || 1 /* UTC */);
}
function addTicks(d, t) {
    return parse(__WEBPACK_IMPORTED_MODULE_0__Long__["a" /* fromNumber */](d.getTime()).add(t.div(10000)).toNumber(), d.kind || 1 /* UTC */);
}
function addYears(d, v) {
    var newMonth = month(d);
    var newYear = year(d) + v;
    var _daysInMonth = daysInMonth(newYear, newMonth);
    var newDay = Math.min(_daysInMonth, day(d));
    return create(newYear, newMonth, newDay, hour(d), minute(d), second(d), millisecond(d), d.kind || 1 /* UTC */);
}
function addMonths(d, v) {
    var newMonth = month(d) + v;
    var newMonth_ = 0;
    var yearOffset = 0;
    if (newMonth > 12) {
        newMonth_ = newMonth % 12;
        yearOffset = Math.floor(newMonth / 12);
        newMonth = newMonth_;
    } else if (newMonth < 1) {
        newMonth_ = 12 + newMonth % 12;
        yearOffset = Math.floor(newMonth / 12) + (newMonth_ === 12 ? -1 : 0);
        newMonth = newMonth_;
    }
    var newYear = year(d) + yearOffset;
    var _daysInMonth = daysInMonth(newYear, newMonth);
    var newDay = Math.min(_daysInMonth, day(d));
    return create(newYear, newMonth, newDay, hour(d), minute(d), second(d), millisecond(d), d.kind || 1 /* UTC */);
}
function subtract(d, that) {
    return typeof that === "number" ? parse(d.getTime() - that, d.kind || 1 /* UTC */) : d.getTime() - that.getTime();
}
function toLongDateString(d) {
    return d.toDateString();
}
function toShortDateString(d) {
    return d.toLocaleDateString();
}
function toLongTimeString(d) {
    return d.toLocaleTimeString();
}
function toShortTimeString(d) {
    return d.toLocaleTimeString().replace(/:\d\d(?!:)/, "");
}
function equals(d1, d2) {
    return d1.getTime() === d2.getTime();
}
function compare(x, y) {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Util__["b" /* compare */])(x, y);
}
function compareTo(x, y) {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Util__["b" /* compare */])(x, y);
}
function op_Addition(x, y) {
    return add(x, y);
}
function op_Subtraction(x, y) {
    return subtract(x, y);
}

/***/ }),
/* 19 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(96), __esModule: true };

/***/ }),
/* 20 */
/***/ (function(module, exports, __webpack_require__) {

// optional / simple context binding
var aFunction = __webpack_require__(111);
module.exports = function(fn, that, length){
  aFunction(fn);
  if(that === undefined)return fn;
  switch(length){
    case 1: return function(a){
      return fn.call(that, a);
    };
    case 2: return function(a, b){
      return fn.call(that, a, b);
    };
    case 3: return function(a, b, c){
      return fn.call(that, a, b, c);
    };
  }
  return function(/* ...args */){
    return fn.apply(that, arguments);
  };
};

/***/ }),
/* 21 */
/***/ (function(module, exports) {

module.exports = {};

/***/ }),
/* 22 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony export (immutable) */ __webpack_exports__["a"] = ofArray;
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_symbol_iterator__ = __webpack_require__(23);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_symbol_iterator___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_symbol_iterator__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_get_iterator__ = __webpack_require__(14);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_get_iterator___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_get_iterator__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_babel_runtime_core_js_array_from__ = __webpack_require__(19);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_babel_runtime_core_js_array_from___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_2_babel_runtime_core_js_array_from__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3_babel_runtime_helpers_classCallCheck__ = __webpack_require__(9);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3_babel_runtime_helpers_classCallCheck___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_3_babel_runtime_helpers_classCallCheck__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4_babel_runtime_helpers_createClass__ = __webpack_require__(10);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4_babel_runtime_helpers_createClass___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_4_babel_runtime_helpers_createClass__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_5__Symbol__ = __webpack_require__(6);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_6__Util__ = __webpack_require__(1);









// This module is split from List.ts to prevent cyclic dependencies
function ofArray(args, base) {
    var acc = base || new List();
    for (var i = args.length - 1; i >= 0; i--) {
        acc = new List(args[i], acc);
    }
    return acc;
}

var List = function () {
    function List(head, tail) {
        __WEBPACK_IMPORTED_MODULE_3_babel_runtime_helpers_classCallCheck___default()(this, List);

        this.head = head;
        this.tail = tail;
    }

    __WEBPACK_IMPORTED_MODULE_4_babel_runtime_helpers_createClass___default()(List, [{
        key: "ToString",
        value: function ToString() {
            return "[" + __WEBPACK_IMPORTED_MODULE_2_babel_runtime_core_js_array_from___default()(this).map(function (x) {
                return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__Util__["a" /* toString */])(x);
            }).join("; ") + "]";
        }
    }, {
        key: "Equals",
        value: function Equals(x) {
            // Optimization if they are referencially equal
            if (this === x) {
                return true;
            } else {
                var iter1 = __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_get_iterator___default()(this);
                var iter2 = __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_get_iterator___default()(x);
                while (true) {
                    var cur1 = iter1.next();
                    var cur2 = iter2.next();
                    if (cur1.done) {
                        return cur2.done ? true : false;
                    } else if (cur2.done) {
                        return false;
                    } else if (!__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__Util__["c" /* equals */])(cur1.value, cur2.value)) {
                        return false;
                    }
                }
            }
        }
    }, {
        key: "CompareTo",
        value: function CompareTo(x) {
            // Optimization if they are referencially equal
            if (this === x) {
                return 0;
            } else {
                var acc = 0;
                var iter1 = __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_get_iterator___default()(this);
                var iter2 = __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_get_iterator___default()(x);
                while (true) {
                    var cur1 = iter1.next();
                    var cur2 = iter2.next();
                    if (cur1.done) {
                        return cur2.done ? acc : -1;
                    } else if (cur2.done) {
                        return 1;
                    } else {
                        acc = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__Util__["b" /* compare */])(cur1.value, cur2.value);
                        if (acc !== 0) {
                            return acc;
                        }
                    }
                }
            }
        }
    }, {
        key: __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_symbol_iterator___default.a,
        value: function value() {
            var cur = this;
            return {
                next: function next() {
                    var tmp = cur;
                    cur = cur.tail;
                    return { done: tmp.tail == null, value: tmp.head };
                }
            };
        }
        //   append(ys: List<T>): List<T> {
        //     return append(this, ys);
        //   }
        //   choose<U>(f: (x: T) => U, xs: List<T>): List<U> {
        //     return choose(f, this);
        //   }
        //   collect<U>(f: (x: T) => List<U>): List<U> {
        //     return collect(f, this);
        //   }
        //   filter(f: (x: T) => boolean): List<T> {
        //     return filter(f, this);
        //   }
        //   where(f: (x: T) => boolean): List<T> {
        //     return filter(f, this);
        //   }
        //   map<U>(f: (x: T) => U): List<U> {
        //     return map(f, this);
        //   }
        //   mapIndexed<U>(f: (i: number, x: T) => U): List<U> {
        //     return mapIndexed(f, this);
        //   }
        //   partition(f: (x: T) => boolean): [List<T>, List<T>] {
        //     return partition(f, this) as [List<T>, List<T>];
        //   }
        //   reverse(): List<T> {
        //     return reverse(this);
        //   }
        //   slice(lower: number, upper: number): List<T> {
        //     return slice(lower, upper, this);
        //   }

    }, {
        key: __WEBPACK_IMPORTED_MODULE_5__Symbol__["a" /* default */].reflection,
        value: function value() {
            return {
                type: "Microsoft.FSharp.Collections.FSharpList",
                interfaces: ["System.IEquatable", "System.IComparable"]
            };
        }
    }, {
        key: "length",
        get: function get() {
            var cur = this;
            var acc = 0;
            while (cur.tail != null) {
                cur = cur.tail;
                acc++;
            }
            return acc;
        }
    }]);

    return List;
}();

/* harmony default export */ __webpack_exports__["b"] = (List);

/***/ }),
/* 23 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(110), __esModule: true };

/***/ }),
/* 24 */
/***/ (function(module, exports, __webpack_require__) {

// 19.1.2.14 / 15.2.3.14 Object.keys(O)
var $keys       = __webpack_require__(78)
  , enumBugKeys = __webpack_require__(43);

module.exports = Object.keys || function keys(O){
  return $keys(O, enumBugKeys);
};

/***/ }),
/* 25 */
/***/ (function(module, exports) {

module.exports = function(bitmap, value){
  return {
    enumerable  : !(bitmap & 1),
    configurable: !(bitmap & 2),
    writable    : !(bitmap & 4),
    value       : value
  };
};

/***/ }),
/* 26 */
/***/ (function(module, exports, __webpack_require__) {

// 7.1.13 ToObject(argument)
var defined = __webpack_require__(31);
module.exports = function(it){
  return Object(defined(it));
};

/***/ }),
/* 27 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

var $at  = __webpack_require__(131)(true);

// 21.1.3.27 String.prototype[@@iterator]()
__webpack_require__(46)(String, 'String', function(iterated){
  this._t = String(iterated); // target
  this._i = 0;                // next index
// 21.1.5.2.1 %StringIteratorPrototype%.next()
}, function(){
  var O     = this._t
    , index = this._i
    , point;
  if(index >= O.length)return {value: undefined, done: true};
  point = $at(O, index);
  this._i += point.length;
  return {value: point, done: false};
});

/***/ }),
/* 28 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony export (immutable) */ __webpack_exports__["f"] = append;
/* harmony export (immutable) */ __webpack_exports__["e"] = choose;
/* unused harmony export collect */
/* unused harmony export concat */
/* unused harmony export filter */
/* unused harmony export where */
/* unused harmony export initialize */
/* harmony export (immutable) */ __webpack_exports__["c"] = map;
/* unused harmony export mapIndexed */
/* unused harmony export partition */
/* unused harmony export replicate */
/* harmony export (immutable) */ __webpack_exports__["d"] = reverse;
/* unused harmony export singleton */
/* unused harmony export slice */
/* unused harmony export unzip */
/* unused harmony export unzip3 */
/* unused harmony export groupBy */
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0__ListClass__ = __webpack_require__(22);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1__Map__ = __webpack_require__(29);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2__Seq__ = __webpack_require__(3);
/* harmony reexport (binding) */ __webpack_require__.d(__webpack_exports__, "b", function() { return __WEBPACK_IMPORTED_MODULE_0__ListClass__["a"]; });






/* harmony default export */ __webpack_exports__["a"] = (__WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */]);

function append(xs, ys) {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Seq__["c" /* fold */])(function (acc, x) {
        return new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](x, acc);
    }, ys, reverse(xs));
}
function choose(f, xs) {
    var r = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Seq__["c" /* fold */])(function (acc, x) {
        var y = f(x);
        return y != null ? new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](y, acc) : acc;
    }, new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](), xs);
    return reverse(r);
}
function collect(f, xs) {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Seq__["c" /* fold */])(function (acc, x) {
        return append(acc, f(x));
    }, new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](), xs);
}
// TODO: should be xs: Iterable<List<T>>
function concat(xs) {
    return collect(function (x) {
        return x;
    }, xs);
}
function filter(f, xs) {
    return reverse(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Seq__["c" /* fold */])(function (acc, x) {
        return f(x) ? new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](x, acc) : acc;
    }, new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](), xs));
}
function where(f, xs) {
    return filter(f, xs);
}
function initialize(n, f) {
    if (n < 0) {
        throw new Error("List length must be non-negative");
    }
    var xs = new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */]();
    for (var i = 1; i <= n; i++) {
        xs = new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](f(n - i), xs);
    }
    return xs;
}
function map(f, xs) {
    return reverse(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Seq__["c" /* fold */])(function (acc, x) {
        return new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](f(x), acc);
    }, new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](), xs));
}
function mapIndexed(f, xs) {
    return reverse(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Seq__["c" /* fold */])(function (acc, x, i) {
        return new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](f(i, x), acc);
    }, new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](), xs));
}
function partition(f, xs) {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Seq__["c" /* fold */])(function (acc, x) {
        var lacc = acc[0];
        var racc = acc[1];
        return f(x) ? [new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](x, lacc), racc] : [lacc, new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](x, racc)];
    }, [new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](), new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */]()], reverse(xs));
}
function replicate(n, x) {
    return initialize(n, function () {
        return x;
    });
}
function reverse(xs) {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Seq__["c" /* fold */])(function (acc, x) {
        return new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](x, acc);
    }, new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](), xs);
}
function singleton(x) {
    return new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](x, new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */]());
}
function slice(lower, upper, xs) {
    var noLower = lower == null;
    var noUpper = upper == null;
    return reverse(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Seq__["c" /* fold */])(function (acc, x, i) {
        return (noLower || lower <= i) && (noUpper || i <= upper) ? new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](x, acc) : acc;
    }, new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](), xs));
}
/* ToDo: instance unzip() */
function unzip(xs) {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Seq__["l" /* foldBack */])(function (xy, acc) {
        return [new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](xy[0], acc[0]), new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](xy[1], acc[1])];
    }, xs, [new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](), new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */]()]);
}
/* ToDo: instance unzip3() */
function unzip3(xs) {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Seq__["l" /* foldBack */])(function (xyz, acc) {
        return [new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](xyz[0], acc[0]), new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](xyz[1], acc[1]), new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](xyz[2], acc[2])];
    }, xs, [new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](), new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */](), new __WEBPACK_IMPORTED_MODULE_0__ListClass__["b" /* default */]()]);
}
function groupBy(f, xs) {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Seq__["d" /* toList */])(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Seq__["g" /* map */])(function (k) {
        return [k[0], __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Seq__["d" /* toList */])(k[1])];
    }, __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_1__Map__["e" /* groupBy */])(f, xs)));
}

/***/ }),
/* 29 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony export (immutable) */ __webpack_exports__["e"] = groupBy;
/* unused harmony export countBy */
/* unused harmony export MapTree */
/* harmony export (immutable) */ __webpack_exports__["c"] = create;
/* harmony export (immutable) */ __webpack_exports__["b"] = add;
/* unused harmony export remove */
/* unused harmony export containsValue */
/* unused harmony export tryGetValue */
/* unused harmony export exists */
/* unused harmony export find */
/* harmony export (immutable) */ __webpack_exports__["a"] = tryFind;
/* unused harmony export filter */
/* unused harmony export fold */
/* unused harmony export foldBack */
/* unused harmony export forAll */
/* unused harmony export isEmpty */
/* unused harmony export iterate */
/* unused harmony export map */
/* unused harmony export partition */
/* unused harmony export findKey */
/* unused harmony export tryFindKey */
/* unused harmony export pick */
/* unused harmony export tryPick */
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_symbol_iterator__ = __webpack_require__(23);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_symbol_iterator___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_symbol_iterator__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from__ = __webpack_require__(19);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_createClass__ = __webpack_require__(10);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_createClass___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_createClass__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3_babel_runtime_helpers_classCallCheck__ = __webpack_require__(9);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3_babel_runtime_helpers_classCallCheck___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_3_babel_runtime_helpers_classCallCheck__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator__ = __webpack_require__(14);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_5__Comparer__ = __webpack_require__(38);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_6__ListClass__ = __webpack_require__(22);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_7__Seq__ = __webpack_require__(3);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_8__Symbol__ = __webpack_require__(6);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_9__Util__ = __webpack_require__(1);

















// ----------------------------------------------
// These functions belong to Seq.ts but are
// implemented here to prevent cyclic dependencies
function groupBy(f, xs) {
    var keys = [];
    var iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
    var acc = create();
    var cur = iter.next();
    while (!cur.done) {
        var k = f(cur.value);
        var vs = tryFind(k, acc);
        if (vs == null) {
            keys.push(k);
            acc = add(k, [cur.value], acc);
        } else {
            vs.push(cur.value);
        }
        cur = iter.next();
    }
    return keys.map(function (k) {
        return [k, acc.get(k)];
    });
}
function countBy(f, xs) {
    return groupBy(f, xs).map(function (kv) {
        return [kv[0], kv[1].length];
    });
}
var MapTree = function MapTree(tag, data) {
    __WEBPACK_IMPORTED_MODULE_3_babel_runtime_helpers_classCallCheck___default()(this, MapTree);

    this.tag = tag | 0;
    this.data = data;
};
function tree_sizeAux(acc, m) {
    sizeAux: while (true) {
        if (m.tag === 1) {
            return acc + 1 | 0;
        } else if (m.tag === 2) {
            acc = tree_sizeAux(acc + 1, m.data[2]);
            m = m.data[3];
            continue sizeAux;
        } else {
            return acc | 0;
        }
    }
}
function tree_size(x) {
    return tree_sizeAux(0, x);
}
function tree_empty() {
    return new MapTree(0);
}
function tree_height(_arg1) {
    return _arg1.tag === 1 ? 1 : _arg1.tag === 2 ? _arg1.data[4] : 0;
}
function tree_isEmpty(m) {
    return m.tag === 0 ? true : false;
}
function tree_mk(l, k, v, r) {
    var matchValue = l.tag === 0 ? r.tag === 0 ? 0 : 1 : 1;
    switch (matchValue) {
        case 0:
            return new MapTree(1, [k, v]);
        case 1:
            var hl = tree_height(l) | 0;
            var hr = tree_height(r) | 0;
            var m = (hl < hr ? hr : hl) | 0;
            return new MapTree(2, [k, v, l, r, m + 1]);
    }
    throw new Error("internal error: Map.tree_mk");
}
function tree_rebalance(t1, k, v, t2) {
    var t1h = tree_height(t1);
    var t2h = tree_height(t2);
    if (t2h > t1h + 2) {
        if (t2.tag === 2) {
            if (tree_height(t2.data[2]) > t1h + 1) {
                if (t2.data[2].tag === 2) {
                    return tree_mk(tree_mk(t1, k, v, t2.data[2].data[2]), t2.data[2].data[0], t2.data[2].data[1], tree_mk(t2.data[2].data[3], t2.data[0], t2.data[1], t2.data[3]));
                } else {
                    throw new Error("rebalance");
                }
            } else {
                return tree_mk(tree_mk(t1, k, v, t2.data[2]), t2.data[0], t2.data[1], t2.data[3]);
            }
        } else {
            throw new Error("rebalance");
        }
    } else {
        if (t1h > t2h + 2) {
            if (t1.tag === 2) {
                if (tree_height(t1.data[3]) > t2h + 1) {
                    if (t1.data[3].tag === 2) {
                        return tree_mk(tree_mk(t1.data[2], t1.data[0], t1.data[1], t1.data[3].data[2]), t1.data[3].data[0], t1.data[3].data[1], tree_mk(t1.data[3].data[3], k, v, t2));
                    } else {
                        throw new Error("rebalance");
                    }
                } else {
                    return tree_mk(t1.data[2], t1.data[0], t1.data[1], tree_mk(t1.data[3], k, v, t2));
                }
            } else {
                throw new Error("rebalance");
            }
        } else {
            return tree_mk(t1, k, v, t2);
        }
    }
}
function tree_add(comparer, k, v, m) {
    if (m.tag === 1) {
        var c = comparer.Compare(k, m.data[0]);
        if (c < 0) {
            return new MapTree(2, [k, v, new MapTree(0), m, 2]);
        } else if (c === 0) {
            return new MapTree(1, [k, v]);
        }
        return new MapTree(2, [k, v, m, new MapTree(0), 2]);
    } else if (m.tag === 2) {
        var _c = comparer.Compare(k, m.data[0]);
        if (_c < 0) {
            return tree_rebalance(tree_add(comparer, k, v, m.data[2]), m.data[0], m.data[1], m.data[3]);
        } else if (_c === 0) {
            return new MapTree(2, [k, v, m.data[2], m.data[3], m.data[4]]);
        }
        return tree_rebalance(m.data[2], m.data[0], m.data[1], tree_add(comparer, k, v, m.data[3]));
    }
    return new MapTree(1, [k, v]);
}
function tree_find(comparer, k, m) {
    var res = tree_tryFind(comparer, k, m);
    if (res != null) {
        return res;
    }
    throw new Error("key not found");
}
function tree_tryFind(comparer, k, m) {
    tryFind: while (true) {
        if (m.tag === 1) {
            var c = comparer.Compare(k, m.data[0]) | 0;
            if (c === 0) {
                return m.data[1];
            } else {
                return null;
            }
        } else if (m.tag === 2) {
            var c_1 = comparer.Compare(k, m.data[0]) | 0;
            if (c_1 < 0) {
                comparer = comparer;
                k = k;
                m = m.data[2];
                continue tryFind;
            } else if (c_1 === 0) {
                return m.data[1];
            } else {
                comparer = comparer;
                k = k;
                m = m.data[3];
                continue tryFind;
            }
        } else {
            return null;
        }
    }
}
function tree_partition1(comparer, f, k, v, acc1, acc2) {
    return f(k, v) ? [tree_add(comparer, k, v, acc1), acc2] : [acc1, tree_add(comparer, k, v, acc2)];
}
function tree_partitionAux(comparer, f, s, acc_0, acc_1) {
    var acc = [acc_0, acc_1];
    if (s.tag === 1) {
        return tree_partition1(comparer, f, s.data[0], s.data[1], acc[0], acc[1]);
    } else if (s.tag === 2) {
        var acc_2 = tree_partitionAux(comparer, f, s.data[3], acc[0], acc[1]);
        var acc_3 = tree_partition1(comparer, f, s.data[0], s.data[1], acc_2[0], acc_2[1]);
        return tree_partitionAux(comparer, f, s.data[2], acc_3[0], acc_3[1]);
    }
    return acc;
}
function tree_partition(comparer, f, s) {
    return tree_partitionAux(comparer, f, s, tree_empty(), tree_empty());
}
function tree_filter1(comparer, f, k, v, acc) {
    return f(k, v) ? tree_add(comparer, k, v, acc) : acc;
}
function tree_filterAux(comparer, f, s, acc) {
    return s.tag === 1 ? tree_filter1(comparer, f, s.data[0], s.data[1], acc) : s.tag === 2 ? tree_filterAux(comparer, f, s.data[3], tree_filter1(comparer, f, s.data[0], s.data[1], tree_filterAux(comparer, f, s.data[2], acc))) : acc;
}
function tree_filter(comparer, f, s) {
    return tree_filterAux(comparer, f, s, tree_empty());
}
function tree_spliceOutSuccessor(m) {
    if (m.tag === 1) {
        return [m.data[0], m.data[1], new MapTree(0)];
    } else if (m.tag === 2) {
        if (m.data[2].tag === 0) {
            return [m.data[0], m.data[1], m.data[3]];
        } else {
            var kvl = tree_spliceOutSuccessor(m.data[2]);
            return [kvl[0], kvl[1], tree_mk(kvl[2], m.data[0], m.data[1], m.data[3])];
        }
    }
    throw new Error("internal error: Map.spliceOutSuccessor");
}
function tree_remove(comparer, k, m) {
    if (m.tag === 1) {
        var c = comparer.Compare(k, m.data[0]);
        if (c === 0) {
            return new MapTree(0);
        } else {
            return m;
        }
    } else if (m.tag === 2) {
        var _c2 = comparer.Compare(k, m.data[0]);
        if (_c2 < 0) {
            return tree_rebalance(tree_remove(comparer, k, m.data[2]), m.data[0], m.data[1], m.data[3]);
        } else if (_c2 === 0) {
            if (m.data[2].tag === 0) {
                return m.data[3];
            } else {
                if (m.data[3].tag === 0) {
                    return m.data[2];
                } else {
                    var input = tree_spliceOutSuccessor(m.data[3]);
                    return tree_mk(m.data[2], input[0], input[1], input[2]);
                }
            }
        } else {
            return tree_rebalance(m.data[2], m.data[0], m.data[1], tree_remove(comparer, k, m.data[3]));
        }
    } else {
        return tree_empty();
    }
}
function tree_mem(comparer, k, m) {
    mem: while (true) {
        if (m.tag === 1) {
            return comparer.Compare(k, m.data[0]) === 0;
        } else if (m.tag === 2) {
            var c = comparer.Compare(k, m.data[0]) | 0;
            if (c < 0) {
                comparer = comparer;
                k = k;
                m = m.data[2];
                continue mem;
            } else if (c === 0) {
                return true;
            } else {
                comparer = comparer;
                k = k;
                m = m.data[3];
                continue mem;
            }
        } else {
            return false;
        }
    }
}
function tree_iter(f, m) {
    if (m.tag === 1) {
        f(m.data[0], m.data[1]);
    } else if (m.tag === 2) {
        tree_iter(f, m.data[2]);
        f(m.data[0], m.data[1]);
        tree_iter(f, m.data[3]);
    }
}
function tree_tryPick(f, m) {
    if (m.tag === 1) {
        return f(m.data[0], m.data[1]);
    } else if (m.tag === 2) {
        var matchValue = tree_tryPick(f, m.data[2]);
        if (matchValue == null) {
            var matchValue_1 = f(m.data[0], m.data[1]);
            if (matchValue_1 == null) {
                return tree_tryPick(f, m.data[3]);
            } else {
                var res = matchValue_1;
                return res;
            }
        } else {
            return matchValue;
        }
    } else {
        return null;
    }
}
function tree_exists(f, m) {
    return m.tag === 1 ? f(m.data[0], m.data[1]) : m.tag === 2 ? (tree_exists(f, m.data[2]) ? true : f(m.data[0], m.data[1])) ? true : tree_exists(f, m.data[3]) : false;
}
function tree_forall(f, m) {
    return m.tag === 1 ? f(m.data[0], m.data[1]) : m.tag === 2 ? (tree_forall(f, m.data[2]) ? f(m.data[0], m.data[1]) : false) ? tree_forall(f, m.data[3]) : false : true;
}
function tree_mapi(f, m) {
    return m.tag === 1 ? new MapTree(1, [m.data[0], f(m.data[0], m.data[1])]) : m.tag === 2 ? new MapTree(2, [m.data[0], f(m.data[0], m.data[1]), tree_mapi(f, m.data[2]), tree_mapi(f, m.data[3]), m.data[4]]) : tree_empty();
}
function tree_foldBack(f, m, x) {
    return m.tag === 1 ? f(m.data[0], m.data[1], x) : m.tag === 2 ? tree_foldBack(f, m.data[2], f(m.data[0], m.data[1], tree_foldBack(f, m.data[3], x))) : x;
}
function tree_fold(f, x, m) {
    return m.tag === 1 ? f(x, m.data[0], m.data[1]) : m.tag === 2 ? tree_fold(f, f(tree_fold(f, x, m.data[2]), m.data[0], m.data[1]), m.data[3]) : x;
}
// function tree_foldFromTo(
//     comparer: IComparer<any>, lo: any, hi: any,
//     f: (k:any, v: any, acc: any) => any, m: MapTree, x: any): any {
//   if (m.tag === 1) {
//     var cLoKey = comparer.Compare(lo, m.data[0]);
//     var cKeyHi = comparer.Compare(m.data[0], hi);
//     var x_1 = (cLoKey <= 0 ? cKeyHi <= 0 : false) ? f(m.data[0], m.data[1], x) : x;
//     return x_1;
//   } else if (m.tag === 2) {
//     var cLoKey = comparer.Compare(lo, m.data[0]);
//     var cKeyHi = comparer.Compare(m.data[0], hi);
//     var x_1 = cLoKey < 0 ? tree_foldFromTo(comparer, lo, hi, f, m.data[2], x) : x;
//     var x_2 = (cLoKey <= 0 ? cKeyHi <= 0 : false) ? f(m.data[0], m.data[1], x_1) : x_1;
//     var x_3 = cKeyHi < 0 ? tree_foldFromTo(comparer, lo, hi, f, m.data[3], x_2) : x_2;
//     return x_3;
//   }
//   return x;
// }
// function tree_foldSection(
//     comparer: IComparer<any>, lo: any, hi: any,
//     f: (k: any, v: any, acc: any) => any, m: MapTree, x: any) {
//   return comparer.Compare(lo, hi) === 1 ? x : tree_foldFromTo(comparer, lo, hi, f, m, x);
// }
// function tree_loop(m: MapTree, acc: any): List<[any,any]> {
//   return m.tag === 1
//     ? new List([m.data[0], m.data[1]], acc)
//     : m.tag === 2
//       ? tree_loop(m.data[2], new List([m.data[0], m.data[1]], tree_loop(m.data[3], acc)))
//       : acc;
// }
// function tree_toList(m: MapTree) {
//   return tree_loop(m, new List());
// }
// function tree_toArray(m: MapTree) {
//   return Array.from(tree_toList(m));
// }
// function tree_ofList(comparer: IComparer<any>, l: List<[any,any]>) {
//   return Seq.fold((acc: MapTree, tupledArg: [any, any]) => {
//     return tree_add(comparer, tupledArg[0], tupledArg[1], acc);
//   }, tree_empty(), l);
// }
function tree_mkFromEnumerator(comparer, acc, e) {
    var cur = e.next();
    while (!cur.done) {
        acc = tree_add(comparer, cur.value[0], cur.value[1], acc);
        cur = e.next();
    }
    return acc;
}
// function tree_ofArray(comparer: IComparer<any>, arr: ArrayLike<[any,any]>) {
//   var res = tree_empty();
//   for (var i = 0; i <= arr.length - 1; i++) {
//     res = tree_add(comparer, arr[i][0], arr[i][1], res);
//   }
//   return res;
// }
function tree_ofSeq(comparer, c) {
    var ie = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(c);
    return tree_mkFromEnumerator(comparer, tree_empty(), ie);
}
// function tree_copyToArray(s: MapTree, arr: ArrayLike<any>, i: number) {
//   tree_iter((x, y) => { arr[i++] = [x, y]; }, s);
// }
function tree_collapseLHS(stack) {
    if (stack.tail != null) {
        if (stack.head.tag === 1) {
            return stack;
        } else if (stack.head.tag === 2) {
            return tree_collapseLHS(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__ListClass__["a" /* ofArray */])([stack.head.data[2], new MapTree(1, [stack.head.data[0], stack.head.data[1]]), stack.head.data[3]], stack.tail));
        } else {
            return tree_collapseLHS(stack.tail);
        }
    } else {
        return new __WEBPACK_IMPORTED_MODULE_6__ListClass__["b" /* default */]();
    }
}
function tree_mkIterator(s) {
    return { stack: tree_collapseLHS(new __WEBPACK_IMPORTED_MODULE_6__ListClass__["b" /* default */](s, new __WEBPACK_IMPORTED_MODULE_6__ListClass__["b" /* default */]())), started: false };
}
function tree_moveNext(i) {
    function current(it) {
        if (it.stack.tail == null) {
            return null;
        } else if (it.stack.head.tag === 1) {
            return [it.stack.head.data[0], it.stack.head.data[1]];
        }
        throw new Error("Please report error: Map iterator, unexpected stack for current");
    }
    if (i.started) {
        if (i.stack.tail == null) {
            return { done: true, value: null };
        } else {
            if (i.stack.head.tag === 1) {
                i.stack = tree_collapseLHS(i.stack.tail);
                return {
                    done: i.stack.tail == null,
                    value: current(i)
                };
            } else {
                throw new Error("Please report error: Map iterator, unexpected stack for moveNext");
            }
        }
    } else {
        i.started = true;
        return {
            done: i.stack.tail == null,
            value: current(i)
        };
    }
}

var FableMap = function () {
    /** Do not call, use Map.create instead. */
    function FableMap() {
        __WEBPACK_IMPORTED_MODULE_3_babel_runtime_helpers_classCallCheck___default()(this, FableMap);

        return;
    }

    __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_createClass___default()(FableMap, [{
        key: "ToString",
        value: function ToString() {
            return "map [" + __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default()(this).map(function (x) {
                return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__Util__["a" /* toString */])(x);
            }).join("; ") + "]";
        }
    }, {
        key: "Equals",
        value: function Equals(m2) {
            return this.CompareTo(m2) === 0;
        }
    }, {
        key: "CompareTo",
        value: function CompareTo(m2) {
            var _this = this;

            return this === m2 ? 0 : __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__Seq__["f" /* compareWith */])(function (kvp1, kvp2) {
                var c = _this.comparer.Compare(kvp1[0], kvp2[0]);
                return c !== 0 ? c : __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__Util__["b" /* compare */])(kvp1[1], kvp2[1]);
            }, this, m2);
        }
    }, {
        key: __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_symbol_iterator___default.a,
        value: function value() {
            var i = tree_mkIterator(this.tree);
            return {
                next: function next() {
                    return tree_moveNext(i);
                }
            };
        }
    }, {
        key: "entries",
        value: function entries() {
            return __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(this);
        }
    }, {
        key: "keys",
        value: function keys() {
            return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__Seq__["g" /* map */])(function (kv) {
                return kv[0];
            }, this);
        }
    }, {
        key: "values",
        value: function values() {
            return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__Seq__["g" /* map */])(function (kv) {
                return kv[1];
            }, this);
        }
    }, {
        key: "get",
        value: function get(k) {
            return tree_find(this.comparer, k, this.tree);
        }
    }, {
        key: "has",
        value: function has(k) {
            return tree_mem(this.comparer, k, this.tree);
        }
        /** Mutating method */

    }, {
        key: "set",
        value: function set(k, v) {
            this.tree = tree_add(this.comparer, k, v, this.tree);
        }
        /** Mutating method */

    }, {
        key: "delete",
        value: function _delete(k) {
            // TODO: Is calculating the size twice is more performant than calling tree_mem?
            var oldSize = tree_size(this.tree);
            this.tree = tree_remove(this.comparer, k, this.tree);
            return oldSize > tree_size(this.tree);
        }
        /** Mutating method */

    }, {
        key: "clear",
        value: function clear() {
            this.tree = tree_empty();
        }
    }, {
        key: __WEBPACK_IMPORTED_MODULE_8__Symbol__["a" /* default */].reflection,
        value: function value() {
            return {
                type: "Microsoft.FSharp.Collections.FSharpMap",
                interfaces: ["System.IEquatable", "System.IComparable", "System.Collections.Generic.IDictionary"]
            };
        }
    }, {
        key: "size",
        get: function get() {
            return tree_size(this.tree);
        }
    }]);

    return FableMap;
}();

/* harmony default export */ __webpack_exports__["d"] = (FableMap);

function from(comparer, tree) {
    var map = new FableMap();
    map.tree = tree;
    map.comparer = comparer || new __WEBPACK_IMPORTED_MODULE_5__Comparer__["a" /* default */]();
    return map;
}
function create(ie, comparer) {
    comparer = comparer || new __WEBPACK_IMPORTED_MODULE_5__Comparer__["a" /* default */]();
    return from(comparer, ie ? tree_ofSeq(comparer, ie) : tree_empty());
}
function add(k, v, map) {
    return from(map.comparer, tree_add(map.comparer, k, v, map.tree));
}
function remove(item, map) {
    return from(map.comparer, tree_remove(map.comparer, item, map.tree));
}
function containsValue(v, map) {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__Seq__["c" /* fold */])(function (acc, k) {
        return acc || __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__Util__["c" /* equals */])(map.get(k), v);
    }, false, map.keys());
}
function tryGetValue(map, key, defaultValue) {
    return map.has(key) ? [true, map.get(key)] : [false, defaultValue];
}
function exists(f, map) {
    return tree_exists(f, map.tree);
}
function find(k, map) {
    return tree_find(map.comparer, k, map.tree);
}
function tryFind(k, map) {
    return tree_tryFind(map.comparer, k, map.tree);
}
function filter(f, map) {
    return from(map.comparer, tree_filter(map.comparer, f, map.tree));
}
function fold(f, seed, map) {
    return tree_fold(f, seed, map.tree);
}
function foldBack(f, map, seed) {
    return tree_foldBack(f, map.tree, seed);
}
function forAll(f, map) {
    return tree_forall(f, map.tree);
}
function isEmpty(map) {
    return tree_isEmpty(map.tree);
}
function iterate(f, map) {
    tree_iter(f, map.tree);
}
function map(f, map) {
    return from(map.comparer, tree_mapi(f, map.tree));
}
function partition(f, map) {
    var rs = tree_partition(map.comparer, f, map.tree);
    return [from(map.comparer, rs[0]), from(map.comparer, rs[1])];
}
function findKey(f, map) {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__Seq__["h" /* pick */])(function (kv) {
        return f(kv[0], kv[1]) ? kv[0] : null;
    }, map);
}
function tryFindKey(f, map) {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__Seq__["i" /* tryPick */])(function (kv) {
        return f(kv[0], kv[1]) ? kv[0] : null;
    }, map);
}
function pick(f, map) {
    var res = tryPick(f, map);
    if (res != null) {
        return res;
    }
    throw new Error("key not found");
}
function tryPick(f, map) {
    return tree_tryPick(f, map.tree);
}

/***/ }),
/* 30 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


exports.__esModule = true;

var _iterator = __webpack_require__(23);

var _iterator2 = _interopRequireDefault(_iterator);

var _symbol = __webpack_require__(66);

var _symbol2 = _interopRequireDefault(_symbol);

var _typeof = typeof _symbol2.default === "function" && typeof _iterator2.default === "symbol" ? function (obj) { return typeof obj; } : function (obj) { return obj && typeof _symbol2.default === "function" && obj.constructor === _symbol2.default && obj !== _symbol2.default.prototype ? "symbol" : typeof obj; };

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

exports.default = typeof _symbol2.default === "function" && _typeof(_iterator2.default) === "symbol" ? function (obj) {
  return typeof obj === "undefined" ? "undefined" : _typeof(obj);
} : function (obj) {
  return obj && typeof _symbol2.default === "function" && obj.constructor === _symbol2.default && obj !== _symbol2.default.prototype ? "symbol" : typeof obj === "undefined" ? "undefined" : _typeof(obj);
};

/***/ }),
/* 31 */
/***/ (function(module, exports) {

// 7.2.1 RequireObjectCoercible(argument)
module.exports = function(it){
  if(it == undefined)throw TypeError("Can't call method on  " + it);
  return it;
};

/***/ }),
/* 32 */
/***/ (function(module, exports, __webpack_require__) {

// 19.1.2.2 / 15.2.3.5 Object.create(O [, Properties])
var anObject    = __webpack_require__(11)
  , dPs         = __webpack_require__(128)
  , enumBugKeys = __webpack_require__(43)
  , IE_PROTO    = __webpack_require__(52)('IE_PROTO')
  , Empty       = function(){ /* empty */ }
  , PROTOTYPE   = 'prototype';

// Create object with fake `null` prototype: use iframe Object with cleared prototype
var createDict = function(){
  // Thrash, waste and sodomy: IE GC bug
  var iframe = __webpack_require__(69)('iframe')
    , i      = enumBugKeys.length
    , lt     = '<'
    , gt     = '>'
    , iframeDocument;
  iframe.style.display = 'none';
  __webpack_require__(123).appendChild(iframe);
  iframe.src = 'javascript:'; // eslint-disable-line no-script-url
  // createDict = iframe.contentWindow.Object;
  // html.removeChild(iframe);
  iframeDocument = iframe.contentWindow.document;
  iframeDocument.open();
  iframeDocument.write(lt + 'script' + gt + 'document.F=Object' + lt + '/script' + gt);
  iframeDocument.close();
  createDict = iframeDocument.F;
  while(i--)delete createDict[PROTOTYPE][enumBugKeys[i]];
  return createDict();
};

module.exports = Object.create || function create(O, Properties){
  var result;
  if(O !== null){
    Empty[PROTOTYPE] = anObject(O);
    result = new Empty;
    Empty[PROTOTYPE] = null;
    // add "__proto__" for Object.getPrototypeOf polyfill
    result[IE_PROTO] = O;
  } else result = createDict();
  return Properties === undefined ? result : dPs(result, Properties);
};


/***/ }),
/* 33 */
/***/ (function(module, exports) {

exports.f = {}.propertyIsEnumerable;

/***/ }),
/* 34 */
/***/ (function(module, exports, __webpack_require__) {

var def = __webpack_require__(5).f
  , has = __webpack_require__(16)
  , TAG = __webpack_require__(2)('toStringTag');

module.exports = function(it, tag, stat){
  if(it && !has(it = stat ? it : it.prototype, TAG))def(it, TAG, {configurable: true, value: tag});
};

/***/ }),
/* 35 */
/***/ (function(module, exports, __webpack_require__) {

// 7.1.15 ToLength
var toInteger = __webpack_require__(54)
  , min       = Math.min;
module.exports = function(it){
  return it > 0 ? min(toInteger(it), 0x1fffffffffffff) : 0; // pow(2, 53) - 1 == 9007199254740991
};

/***/ }),
/* 36 */
/***/ (function(module, exports) {

var id = 0
  , px = Math.random();
module.exports = function(key){
  return 'Symbol('.concat(key === undefined ? '' : key, ')_', (++id + px).toString(36));
};

/***/ }),
/* 37 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(136);
var global        = __webpack_require__(8)
  , hide          = __webpack_require__(12)
  , Iterators     = __webpack_require__(21)
  , TO_STRING_TAG = __webpack_require__(2)('toStringTag');

for(var collections = ['NodeList', 'DOMTokenList', 'MediaList', 'StyleSheetList', 'CSSRuleList'], i = 0; i < 5; i++){
  var NAME       = collections[i]
    , Collection = global[NAME]
    , proto      = Collection && Collection.prototype;
  if(proto && !proto[TO_STRING_TAG])hide(proto, TO_STRING_TAG, NAME);
  Iterators[NAME] = Iterators.Array;
}

/***/ }),
/* 38 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* unused harmony export fromEqualityComparer */
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_classCallCheck__ = __webpack_require__(9);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_classCallCheck___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_classCallCheck__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_createClass__ = __webpack_require__(10);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_createClass___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_createClass__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2__Symbol__ = __webpack_require__(6);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3__Util__ = __webpack_require__(1);





var Comparer = function () {
    function Comparer(f) {
        __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_classCallCheck___default()(this, Comparer);

        this.Compare = f || __WEBPACK_IMPORTED_MODULE_3__Util__["b" /* compare */];
    }

    __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_createClass___default()(Comparer, [{
        key: __WEBPACK_IMPORTED_MODULE_2__Symbol__["a" /* default */].reflection,
        value: function value() {
            return { interfaces: ["System.IComparer"] };
        }
    }]);

    return Comparer;
}();

/* harmony default export */ __webpack_exports__["a"] = (Comparer);

function fromEqualityComparer(comparer) {
    // Sometimes IEqualityComparer also implements IComparer
    if (typeof comparer.Compare === "function") {
        return new Comparer(comparer.Compare);
    } else {
        return new Comparer(function (x, y) {
            var xhash = comparer.GetHashCode(x);
            var yhash = comparer.GetHashCode(y);
            if (xhash === yhash) {
                return comparer.Equals(x, y) ? 0 : -1;
            } else {
                return xhash < yhash ? -1 : 1;
            }
        });
    }
}

/***/ }),
/* 39 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* unused harmony export Long */
/* unused harmony export isLong */
/* unused harmony export fromInt */
/* harmony export (immutable) */ __webpack_exports__["a"] = fromNumber;
/* unused harmony export fromBits */
/* harmony export (immutable) */ __webpack_exports__["b"] = fromString;
/* unused harmony export fromValue */
/* unused harmony export ZERO */
/* unused harmony export UZERO */
/* unused harmony export ONE */
/* unused harmony export UONE */
/* unused harmony export NEG_ONE */
/* unused harmony export MAX_VALUE */
/* unused harmony export MAX_UNSIGNED_VALUE */
/* unused harmony export MIN_VALUE */
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_classCallCheck__ = __webpack_require__(9);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_classCallCheck___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_classCallCheck__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_createClass__ = __webpack_require__(10);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_createClass___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_createClass__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2__Int32__ = __webpack_require__(84);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3__Symbol__ = __webpack_require__(6);


// Source: https://github.com/dcodeIO/long.js/blob/master/LICENSE
// tslint:disable:curly
// tslint:disable:member-access
// tslint:disable:member-ordering


// The internal representation of a long is the two given signed, 32-bit values.
// We use 32-bit pieces because these are the size of integers on which
// Javascript performs bit-operations.  For operations like addition and
// multiplication, we split each number into 16 bit pieces, which can easily be
// multiplied within Javascript's floating-point representation without overflow
// or change in sign.
//
// In the algorithms below, we frequently reduce the negative case to the
// positive case by negating the input(s) and then post-processing the result.
// Note that we must ALWAYS check specially whether those values are MIN_VALUE
// (-2^63) because -MIN_VALUE == MIN_VALUE (since 2^63 cannot be represented as
// a positive number, it overflows back into a negative).  Not handling this
// case would often result in infinite recursion.
//
// Common constant values ZERO, ONE, NEG_ONE, etc. are defined below the from*
// methods on which they depend.
/**
 * @class A Long class for representing a 64 bit two's-complement integer value.
 */
var Long = function () {
    /**
     * Constructs a 64 bit two's-complement integer, given its low and high 32 bit values as *signed* integers.
     *  See the from* functions below for more convenient ways of constructing Longs.
     * @param {number} low The low (signed) 32 bits of the long
     * @param {number} high The high (signed) 32 bits of the long
     * @param {boolean=} unsigned Whether unsigned or not, defaults to `false` for signed
     */
    function Long(low, high, unsigned) {
        __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_classCallCheck___default()(this, Long);

        /**
         * Tests if this Long's value equals the specified's. This is an alias of {@link Long#equals}.
         * @param {!Long|number|string} other Other value
         * @returns {boolean}
         */
        this.eq = this.equals;
        /**
         * Tests if this Long's value differs from the specified's. This is an alias of {@link Long#notEquals}.
         * @param {!Long|number|string} other Other value
         * @returns {boolean}
         */
        this.neq = this.notEquals;
        /**
         * Tests if this Long's value is less than the specified's. This is an alias of {@link Long#lessThan}.
         * @param {!Long|number|string} other Other value
         * @returns {boolean}
         */
        this.lt = this.lessThan;
        /**
         * Tests if this Long's value is less than or equal the specified's.
         * This is an alias of {@link Long#lessThanOrEqual}.
         * @param {!Long|number|string} other Other value
         * @returns {boolean}
         */
        this.lte = this.lessThanOrEqual;
        /**
         * Tests if this Long's value is greater than the specified's. This is an alias of {@link Long#greaterThan}.
         * @param {!Long|number|string} other Other value
         * @returns {boolean}
         */
        this.gt = this.greaterThan;
        /**
         * Tests if this Long's value is greater than or equal the specified's.
         * This is an alias of {@link Long#greaterThanOrEqual}.
         * @param {!Long|number|string} other Other value
         * @returns {boolean}
         */
        this.gte = this.greaterThanOrEqual;
        /**
         * Compares this Long's value with the specified's. This is an alias of {@link Long#compare}.
         * @param {!Long|number|string} other Other value
         * @returns {number} 0 if they are the same, 1 if the this is greater and -1
         *  if the given one is greater
         */
        this.comp = this.compare;
        /**
         * Negates this Long's value. This is an alias of {@link Long#negate}.
         * @returns {!Long} Negated Long
         */
        this.neg = this.negate;
        /**
         * Returns this Long's absolute value. This is an alias of {@link Long#absolute}.
         * @returns {!Long} Absolute Long
         */
        this.abs = this.absolute;
        /**
         * Returns the difference of this and the specified  This is an alias of {@link Long#subtract}.
         * @param {!Long|number|string} subtrahend Subtrahend
         * @returns {!Long} Difference
         */
        this.sub = this.subtract;
        /**
         * Returns the product of this and the specified  This is an alias of {@link Long#multiply}.
         * @param {!Long|number|string} multiplier Multiplier
         * @returns {!Long} Product
         */
        this.mul = this.multiply;
        /**
         * Returns this Long divided by the specified. This is an alias of {@link Long#divide}.
         * @param {!Long|number|string} divisor Divisor
         * @returns {!Long} Quotient
         */
        this.div = this.divide;
        /**
         * Returns this Long modulo the specified. This is an alias of {@link Long#modulo}.
         * @param {!Long|number|string} divisor Divisor
         * @returns {!Long} Remainder
         */
        this.mod = this.modulo;
        /**
         * Returns this Long with bits shifted to the left by the given amount. This is an alias of {@link Long#shiftLeft}.
         * @param {number|!Long} numBits Number of bits
         * @returns {!Long} Shifted Long
         */
        this.shl = this.shiftLeft;
        /**
         * Returns this Long with bits arithmetically shifted to the right by the given amount.
         * This is an alias of {@link Long#shiftRight}.
         * @param {number|!Long} numBits Number of bits
         * @returns {!Long} Shifted Long
         */
        this.shr = this.shiftRight;
        /**
         * Returns this Long with bits logically shifted to the right by the given amount.
         * This is an alias of {@link Long#shiftRightUnsigned}.
         * @param {number|!Long} numBits Number of bits
         * @returns {!Long} Shifted Long
         */
        this.shru = this.shiftRightUnsigned;
        // Aliases for compatibility with Fable
        this.Equals = this.equals;
        this.CompareTo = this.compare;
        this.ToString = this.toString;
        this.low = low | 0;
        this.high = high | 0;
        this.unsigned = !!unsigned;
    }
    /**
     * Converts the Long to a 32 bit integer, assuming it is a 32 bit integer.
     * @returns {number}
     */


    __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_createClass___default()(Long, [{
        key: "toInt",
        value: function toInt() {
            return this.unsigned ? this.low >>> 0 : this.low;
        }
        /**
         * Converts the Long to a the nearest floating-point representation of this value (double, 53 bit mantissa).
         * @returns {number}
         */

    }, {
        key: "toNumber",
        value: function toNumber() {
            if (this.unsigned) return (this.high >>> 0) * TWO_PWR_32_DBL + (this.low >>> 0);
            return this.high * TWO_PWR_32_DBL + (this.low >>> 0);
        }
        /**
         * Converts the Long to a string written in the specified radix.
         * @param {number=} radix Radix (2-36), defaults to 10
         * @returns {string}
         * @override
         * @throws {RangeError} If `radix` is out of range
         */

    }, {
        key: "toString",
        value: function toString() {
            var radix = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 10;

            radix = radix || 10;
            if (radix < 2 || 36 < radix) throw RangeError("radix");
            if (this.isZero()) return "0";
            if (this.isNegative()) {
                if (this.eq(MIN_VALUE)) {
                    // We need to change the Long value before it can be negated, so we remove
                    // the bottom-most digit in this base and then recurse to do the rest.
                    var radixLong = fromNumber(radix);
                    var div = this.div(radixLong);
                    var rem1 = div.mul(radixLong).sub(this);
                    return div.toString(radix) + rem1.toInt().toString(radix);
                } else return "-" + this.neg().toString(radix);
            }
            // Do several (6) digits each time through the loop, so as to
            // minimize the calls to the very expensive emulated div.
            var radixToPower = fromNumber(pow_dbl(radix, 6), this.unsigned);
            var rem = this;
            var result = "";
            while (true) {
                var remDiv = rem.div(radixToPower);
                var intval = rem.sub(remDiv.mul(radixToPower)).toInt() >>> 0;
                var digits = intval.toString(radix);
                rem = remDiv;
                if (rem.isZero()) return digits + result;else {
                    while (digits.length < 6) {
                        digits = "0" + digits;
                    }result = "" + digits + result;
                }
            }
        }
        /**
         * Gets the high 32 bits as a signed integer.
         * @returns {number} Signed high bits
         */

    }, {
        key: "getHighBits",
        value: function getHighBits() {
            return this.high;
        }
        /**
         * Gets the high 32 bits as an unsigned integer.
         * @returns {number} Unsigned high bits
         */

    }, {
        key: "getHighBitsUnsigned",
        value: function getHighBitsUnsigned() {
            return this.high >>> 0;
        }
        /**
         * Gets the low 32 bits as a signed integer.
         * @returns {number} Signed low bits
         */

    }, {
        key: "getLowBits",
        value: function getLowBits() {
            return this.low;
        }
        /**
         * Gets the low 32 bits as an unsigned integer.
         * @returns {number} Unsigned low bits
         */

    }, {
        key: "getLowBitsUnsigned",
        value: function getLowBitsUnsigned() {
            return this.low >>> 0;
        }
        /**
         * Gets the number of bits needed to represent the absolute value of this
         * @returns {number}
         */

    }, {
        key: "getNumBitsAbs",
        value: function getNumBitsAbs() {
            if (this.isNegative()) return this.eq(MIN_VALUE) ? 64 : this.neg().getNumBitsAbs();
            var val = this.high !== 0 ? this.high : this.low;
            var bit = void 0;
            for (bit = 31; bit > 0; bit--) {
                if ((val & 1 << bit) !== 0) break;
            }return this.high !== 0 ? bit + 33 : bit + 1;
        }
        /**
         * Tests if this Long's value equals zero.
         * @returns {boolean}
         */

    }, {
        key: "isZero",
        value: function isZero() {
            return this.high === 0 && this.low === 0;
        }
        /**
         * Tests if this Long's value is negative.
         * @returns {boolean}
         */

    }, {
        key: "isNegative",
        value: function isNegative() {
            return !this.unsigned && this.high < 0;
        }
        /**
         * Tests if this Long's value is positive.
         * @returns {boolean}
         */

    }, {
        key: "isPositive",
        value: function isPositive() {
            return this.unsigned || this.high >= 0;
        }
        /**
         * Tests if this Long's value is odd.
         * @returns {boolean}
         */

    }, {
        key: "isOdd",
        value: function isOdd() {
            return (this.low & 1) === 1;
        }
        /**
         * Tests if this Long's value is even.
         * @returns {boolean}
         */

    }, {
        key: "isEven",
        value: function isEven() {
            return (this.low & 1) === 0;
        }
        /**
         * Tests if this Long's value equals the specified's.
         * @param {!Long|number|string} other Other value
         * @returns {boolean}
         */

    }, {
        key: "equals",
        value: function equals(other) {
            if (!isLong(other)) other = fromValue(other);
            if (this.unsigned !== other.unsigned && this.high >>> 31 === 1 && other.high >>> 31 === 1) return false;
            return this.high === other.high && this.low === other.low;
        }
        /**
         * Tests if this Long's value differs from the specified's.
         * @param {!Long|number|string} other Other value
         * @returns {boolean}
         */

    }, {
        key: "notEquals",
        value: function notEquals(other) {
            return !this.eq( /* validates */other);
        }
        /**
         * Tests if this Long's value is less than the specified's.
         * @param {!Long|number|string} other Other value
         * @returns {boolean}
         */

    }, {
        key: "lessThan",
        value: function lessThan(other) {
            return this.comp( /* validates */other) < 0;
        }
        /**
         * Tests if this Long's value is less than or equal the specified's.
         * @param {!Long|number|string} other Other value
         * @returns {boolean}
         */

    }, {
        key: "lessThanOrEqual",
        value: function lessThanOrEqual(other) {
            return this.comp( /* validates */other) <= 0;
        }
        /**
         * Tests if this Long's value is greater than the specified's.
         * @param {!Long|number|string} other Other value
         * @returns {boolean}
         */

    }, {
        key: "greaterThan",
        value: function greaterThan(other) {
            return this.comp( /* validates */other) > 0;
        }
        /**
         * Tests if this Long's value is greater than or equal the specified's.
         * @param {!Long|number|string} other Other value
         * @returns {boolean}
         */

    }, {
        key: "greaterThanOrEqual",
        value: function greaterThanOrEqual(other) {
            return this.comp( /* validates */other) >= 0;
        }
        /**
         * Compares this Long's value with the specified's.
         * @param {!Long|number|string} other Other value
         * @returns {number} 0 if they are the same, 1 if the this is greater and -1
         *  if the given one is greater
         */

    }, {
        key: "compare",
        value: function compare(other) {
            if (!isLong(other)) other = fromValue(other);
            if (this.eq(other)) return 0;
            var thisNeg = this.isNegative();
            var otherNeg = other.isNegative();
            if (thisNeg && !otherNeg) return -1;
            if (!thisNeg && otherNeg) return 1;
            // At this point the sign bits are the same
            if (!this.unsigned) return this.sub(other).isNegative() ? -1 : 1;
            // Both are positive if at least one is unsigned
            return other.high >>> 0 > this.high >>> 0 || other.high === this.high && other.low >>> 0 > this.low >>> 0 ? -1 : 1;
        }
        /**
         * Negates this Long's value.
         * @returns {!Long} Negated Long
         */

    }, {
        key: "negate",
        value: function negate() {
            if (!this.unsigned && this.eq(MIN_VALUE)) return MIN_VALUE;
            return this.not().add(ONE);
        }
        /**
         * Returns this Long's absolute value.
         * @returns {!Long} Absolute Long
         */

    }, {
        key: "absolute",
        value: function absolute() {
            if (!this.unsigned && this.isNegative()) return this.negate();else return this;
        }
        /**
         * Returns the sum of this and the specified
         * @param {!Long|number|string} addend Addend
         * @returns {!Long} Sum
         */

    }, {
        key: "add",
        value: function add(addend) {
            if (!isLong(addend)) addend = fromValue(addend);
            // Divide each number into 4 chunks of 16 bits, and then sum the chunks.
            var a48 = this.high >>> 16;
            var a32 = this.high & 0xFFFF;
            var a16 = this.low >>> 16;
            var a00 = this.low & 0xFFFF;
            var b48 = addend.high >>> 16;
            var b32 = addend.high & 0xFFFF;
            var b16 = addend.low >>> 16;
            var b00 = addend.low & 0xFFFF;
            var c48 = 0;
            var c32 = 0;
            var c16 = 0;
            var c00 = 0;
            c00 += a00 + b00;
            c16 += c00 >>> 16;
            c00 &= 0xFFFF;
            c16 += a16 + b16;
            c32 += c16 >>> 16;
            c16 &= 0xFFFF;
            c32 += a32 + b32;
            c48 += c32 >>> 16;
            c32 &= 0xFFFF;
            c48 += a48 + b48;
            c48 &= 0xFFFF;
            return fromBits(c16 << 16 | c00, c48 << 16 | c32, this.unsigned);
        }
        /**
         * Returns the difference of this and the specified
         * @param {!Long|number|string} subtrahend Subtrahend
         * @returns {!Long} Difference
         */

    }, {
        key: "subtract",
        value: function subtract(subtrahend) {
            if (!isLong(subtrahend)) subtrahend = fromValue(subtrahend);
            return this.add(subtrahend.neg());
        }
        /**
         * Returns the product of this and the specified
         * @param {!Long|number|string} multiplier Multiplier
         * @returns {!Long} Product
         */

    }, {
        key: "multiply",
        value: function multiply(multiplier) {
            if (this.isZero()) return ZERO;
            if (!isLong(multiplier)) multiplier = fromValue(multiplier);
            if (multiplier.isZero()) return ZERO;
            if (this.eq(MIN_VALUE)) return multiplier.isOdd() ? MIN_VALUE : ZERO;
            if (multiplier.eq(MIN_VALUE)) return this.isOdd() ? MIN_VALUE : ZERO;
            if (this.isNegative()) {
                if (multiplier.isNegative()) return this.neg().mul(multiplier.neg());else return this.neg().mul(multiplier).neg();
            } else if (multiplier.isNegative()) return this.mul(multiplier.neg()).neg();
            // If both longs are small, use float multiplication
            if (this.lt(TWO_PWR_24) && multiplier.lt(TWO_PWR_24)) return fromNumber(this.toNumber() * multiplier.toNumber(), this.unsigned);
            // Divide each long into 4 chunks of 16 bits, and then add up 4x4 products.
            // We can skip products that would overflow.
            var a48 = this.high >>> 16;
            var a32 = this.high & 0xFFFF;
            var a16 = this.low >>> 16;
            var a00 = this.low & 0xFFFF;
            var b48 = multiplier.high >>> 16;
            var b32 = multiplier.high & 0xFFFF;
            var b16 = multiplier.low >>> 16;
            var b00 = multiplier.low & 0xFFFF;
            var c48 = 0;
            var c32 = 0;
            var c16 = 0;
            var c00 = 0;
            c00 += a00 * b00;
            c16 += c00 >>> 16;
            c00 &= 0xFFFF;
            c16 += a16 * b00;
            c32 += c16 >>> 16;
            c16 &= 0xFFFF;
            c16 += a00 * b16;
            c32 += c16 >>> 16;
            c16 &= 0xFFFF;
            c32 += a32 * b00;
            c48 += c32 >>> 16;
            c32 &= 0xFFFF;
            c32 += a16 * b16;
            c48 += c32 >>> 16;
            c32 &= 0xFFFF;
            c32 += a00 * b32;
            c48 += c32 >>> 16;
            c32 &= 0xFFFF;
            c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
            c48 &= 0xFFFF;
            return fromBits(c16 << 16 | c00, c48 << 16 | c32, this.unsigned);
        }
        /**
         * Returns this Long divided by the specified. The result is signed if this Long is signed or
         *  unsigned if this Long is unsigned.
         * @param {!Long|number|string} divisor Divisor
         * @returns {!Long} Quotient
         */

    }, {
        key: "divide",
        value: function divide(divisor) {
            if (!isLong(divisor)) divisor = fromValue(divisor);
            if (divisor.isZero()) throw Error("division by zero");
            if (this.isZero()) return this.unsigned ? UZERO : ZERO;
            var rem = ZERO;
            var res = ZERO;
            if (!this.unsigned) {
                // This section is only relevant for signed longs and is derived from the
                // closure library as a whole.
                if (this.eq(MIN_VALUE)) {
                    if (divisor.eq(ONE) || divisor.eq(NEG_ONE)) return MIN_VALUE; // recall that -MIN_VALUE == MIN_VALUE
                    else if (divisor.eq(MIN_VALUE)) return ONE;else {
                            // At this point, we have |other| >= 2, so |this/other| < |MIN_VALUE|.
                            var halfThis = this.shr(1);
                            var approx = halfThis.div(divisor).shl(1);
                            if (approx.eq(ZERO)) {
                                return divisor.isNegative() ? ONE : NEG_ONE;
                            } else {
                                rem = this.sub(divisor.mul(approx));
                                res = approx.add(rem.div(divisor));
                                return res;
                            }
                        }
                } else if (divisor.eq(MIN_VALUE)) return this.unsigned ? UZERO : ZERO;
                if (this.isNegative()) {
                    if (divisor.isNegative()) return this.neg().div(divisor.neg());
                    return this.neg().div(divisor).neg();
                } else if (divisor.isNegative()) return this.div(divisor.neg()).neg();
                res = ZERO;
            } else {
                // The algorithm below has not been made for unsigned longs. It's therefore
                // required to take special care of the MSB prior to running it.
                if (!divisor.unsigned) divisor = divisor.toUnsigned();
                if (divisor.gt(this)) return UZERO;
                if (divisor.gt(this.shru(1))) return UONE;
                res = UZERO;
            }
            // Repeat the following until the remainder is less than other:  find a
            // floating-point that approximates remainder / other *from below*, add this
            // into the result, and subtract it from the remainder.  It is critical that
            // the approximate value is less than or equal to the real value so that the
            // remainder never becomes negative.
            rem = this;
            while (rem.gte(divisor)) {
                // Approximate the result of division. This may be a little greater or
                // smaller than the actual value.
                var _approx = Math.max(1, Math.floor(rem.toNumber() / divisor.toNumber()));
                // We will tweak the approximate result by changing it in the 48-th digit or
                // the smallest non-fractional digit, whichever is larger.
                // tslint:disable-next-line:prefer-const
                // tslint:disable-next-line:semicolon
                var log2 = Math.ceil(Math.log(_approx) / Math.LN2);
                var delta = log2 <= 48 ? 1 : pow_dbl(2, log2 - 48);
                // Decrease the approximation until it is smaller than the remainder.  Note
                // that if it is too large, the product overflows and is negative.
                var approxRes = fromNumber(_approx);
                var approxRem = approxRes.mul(divisor);
                while (approxRem.isNegative() || approxRem.gt(rem)) {
                    _approx -= delta;
                    approxRes = fromNumber(_approx, this.unsigned);
                    approxRem = approxRes.mul(divisor);
                }
                // We know the answer can't be zero... and actually, zero would cause
                // infinite recursion since we would make no progress.
                if (approxRes.isZero()) approxRes = ONE;
                res = res.add(approxRes);
                rem = rem.sub(approxRem);
            }
            return res;
        }
        /**
         * Returns this Long modulo the specified.
         * @param {!Long|number|string} divisor Divisor
         * @returns {!Long} Remainder
         */

    }, {
        key: "modulo",
        value: function modulo(divisor) {
            if (!isLong(divisor)) divisor = fromValue(divisor);
            return this.sub(this.div(divisor).mul(divisor));
        }
        /**
         * Returns the bitwise NOT of this
         * @returns {!Long}
         */

    }, {
        key: "not",
        value: function not() {
            return fromBits(~this.low, ~this.high, this.unsigned);
        }
        /**
         * Returns the bitwise AND of this Long and the specified.
         * @param {!Long|number|string} other Other Long
         * @returns {!Long}
         */

    }, {
        key: "and",
        value: function and(other) {
            if (!isLong(other)) other = fromValue(other);
            return fromBits(this.low & other.low, this.high & other.high, this.unsigned);
        }
        /**
         * Returns the bitwise OR of this Long and the specified.
         * @param {!Long|number|string} other Other Long
         * @returns {!Long}
         */

    }, {
        key: "or",
        value: function or(other) {
            if (!isLong(other)) other = fromValue(other);
            return fromBits(this.low | other.low, this.high | other.high, this.unsigned);
        }
        /**
         * Returns the bitwise XOR of this Long and the given one.
         * @param {!Long|number|string} other Other Long
         * @returns {!Long}
         */

    }, {
        key: "xor",
        value: function xor(other) {
            if (!isLong(other)) other = fromValue(other);
            return fromBits(this.low ^ other.low, this.high ^ other.high, this.unsigned);
        }
        /**
         * Returns this Long with bits shifted to the left by the given amount.
         * @param {number|!Long} numBits Number of bits
         * @returns {!Long} Shifted Long
         */

    }, {
        key: "shiftLeft",
        value: function shiftLeft(numBits) {
            if (isLong(numBits)) numBits = numBits.toInt();
            numBits = numBits & 63;
            if (numBits === 0) return this;else if (numBits < 32) return fromBits(this.low << numBits, this.high << numBits | this.low >>> 32 - numBits, this.unsigned);else return fromBits(0, this.low << numBits - 32, this.unsigned);
        }
        /**
         * Returns this Long with bits arithmetically shifted to the right by the given amount.
         * @param {number|!Long} numBits Number of bits
         * @returns {!Long} Shifted Long
         */

    }, {
        key: "shiftRight",
        value: function shiftRight(numBits) {
            if (isLong(numBits)) numBits = numBits.toInt();
            numBits = numBits & 63;
            if (numBits === 0) return this;else if (numBits < 32) return fromBits(this.low >>> numBits | this.high << 32 - numBits, this.high >> numBits, this.unsigned);else return fromBits(this.high >> numBits - 32, this.high >= 0 ? 0 : -1, this.unsigned);
        }
        /**
         * Returns this Long with bits logically shifted to the right by the given amount.
         * @param {number|!Long} numBits Number of bits
         * @returns {!Long} Shifted Long
         */

    }, {
        key: "shiftRightUnsigned",
        value: function shiftRightUnsigned(numBits) {
            if (isLong(numBits)) numBits = numBits.toInt();
            numBits = numBits & 63;
            if (numBits === 0) return this;else {
                var high = this.high;
                if (numBits < 32) {
                    var low = this.low;
                    return fromBits(low >>> numBits | high << 32 - numBits, high >>> numBits, this.unsigned);
                } else if (numBits === 32) return fromBits(high, 0, this.unsigned);else return fromBits(high >>> numBits - 32, 0, this.unsigned);
            }
        }
        /**
         * Converts this Long to signed.
         * @returns {!Long} Signed long
         */

    }, {
        key: "toSigned",
        value: function toSigned() {
            if (!this.unsigned) return this;
            return fromBits(this.low, this.high, false);
        }
        /**
         * Converts this Long to unsigned.
         * @returns {!Long} Unsigned long
         */

    }, {
        key: "toUnsigned",
        value: function toUnsigned() {
            if (this.unsigned) return this;
            return fromBits(this.low, this.high, true);
        }
        /**
         * Converts this Long to its byte representation.
         * @param {boolean=} le Whether little or big endian, defaults to big endian
         * @returns {!Array.<number>} Byte representation
         */

    }, {
        key: "toBytes",
        value: function toBytes(le) {
            return le ? this.toBytesLE() : this.toBytesBE();
        }
        /**
         * Converts this Long to its little endian byte representation.
         * @returns {!Array.<number>} Little endian byte representation
         */

    }, {
        key: "toBytesLE",
        value: function toBytesLE() {
            var hi = this.high;
            var lo = this.low;
            return [lo & 0xff, lo >>> 8 & 0xff, lo >>> 16 & 0xff, lo >>> 24 & 0xff, hi & 0xff, hi >>> 8 & 0xff, hi >>> 16 & 0xff, hi >>> 24 & 0xff];
        }
        /**
         * Converts this Long to its big endian byte representation.
         * @returns {!Array.<number>} Big endian byte representation
         */

    }, {
        key: "toBytesBE",
        value: function toBytesBE() {
            var hi = this.high;
            var lo = this.low;
            return [hi >>> 24 & 0xff, hi >>> 16 & 0xff, hi >>> 8 & 0xff, hi & 0xff, lo >>> 24 & 0xff, lo >>> 16 & 0xff, lo >>> 8 & 0xff, lo & 0xff];
        }
    }, {
        key: __WEBPACK_IMPORTED_MODULE_3__Symbol__["a" /* default */].reflection,
        value: function value() {
            return {
                type: "System.Int64",
                interfaces: ["FSharpRecord", "System.IComparable"],
                properties: {
                    low: "number",
                    high: "number",
                    unsigned: "boolean"
                }
            };
        }
    }]);

    return Long;
}();
// A cache of the Long representations of small integer values.
var INT_CACHE = {};
// A cache of the Long representations of small unsigned integer values.
var UINT_CACHE = {};
/**
 * Tests if the specified object is a
 * @param {*} obj Object
 * @returns {boolean}
 */
function isLong(obj) {
    return obj && obj instanceof Long;
}
/**
 * Returns a Long representing the given 32 bit integer value.
 * @param {number} value The 32 bit integer in question
 * @param {boolean=} unsigned Whether unsigned or not, defaults to `false` for signed
 * @returns {!Long} The corresponding Long value
 */
function fromInt(value) {
    var unsigned = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : false;

    var obj = void 0;
    var cachedObj = void 0;
    var cache = false;
    if (unsigned) {
        value >>>= 0;
        if (0 <= value && value < 256) {
            cache = true;
            cachedObj = UINT_CACHE[value];
            if (cachedObj) return cachedObj;
        }
        obj = fromBits(value, (value | 0) < 0 ? -1 : 0, true);
        if (cache) UINT_CACHE[value] = obj;
        return obj;
    } else {
        value |= 0;
        if (-128 <= value && value < 128) {
            cache = true;
            cachedObj = INT_CACHE[value];
            if (cachedObj) return cachedObj;
        }
        obj = fromBits(value, value < 0 ? -1 : 0, false);
        if (cache) INT_CACHE[value] = obj;
        return obj;
    }
}
/**
 * Returns a Long representing the given value, provided that it is a finite number. Otherwise, zero is returned.
 * @param {number} value The number in question
 * @param {boolean=} unsigned Whether unsigned or not, defaults to `false` for signed
 * @returns {!Long} The corresponding Long value
 */
function fromNumber(value) {
    var unsigned = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : false;

    if (isNaN(value) || !isFinite(value)) {
        // TODO FormatException ?
        throw new Error("Input string was not in a correct format.");
    }
    if (unsigned) {
        if (value < 0) return UZERO;
        if (value >= TWO_PWR_64_DBL) return MAX_UNSIGNED_VALUE;
    } else {
        if (value <= -TWO_PWR_63_DBL) return MIN_VALUE;
        if (value + 1 >= TWO_PWR_63_DBL) return MAX_VALUE;
    }
    if (value < 0) return fromNumber(-value, unsigned).neg();
    return fromBits(value % TWO_PWR_32_DBL | 0, value / TWO_PWR_32_DBL | 0, unsigned);
}
/**
 * Returns a Long representing the 64 bit integer that comes by concatenating the given low and high bits. Each is
 *  assumed to use 32 bits.
 * @param {number} lowBits The low 32 bits
 * @param {number} highBits The high 32 bits
 * @param {boolean=} unsigned Whether unsigned or not, defaults to `false` for signed
 * @returns {!Long} The corresponding Long value
 */
function fromBits(lowBits, highBits, unsigned) {
    return new Long(lowBits, highBits, unsigned);
}
/**
 * @param {number} base
 * @param {number} exponent
 * @returns {number}
 */
var pow_dbl = Math.pow; // Used 4 times (4*8 to 15+4)
/**
 * Returns a Long representation of the given string, written using the specified radix.
 * @param {string} str The textual representation of the Long
 * @param {(boolean|number)=} unsigned Whether unsigned or not, defaults to `false` for signed
 * @param {number=} radix The radix in which the text is written (2-36), defaults to 10
 * @returns {!Long} The corresponding Long value
 */
// Used 4 times (4*8 to 15+4)
function fromString(str) {
    var unsigned = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : false;
    var radix = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 10;

    if (__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Int32__["a" /* isValid */])(str, radix) === null) {
        // TODO FormatException ?
        throw new Error("Input string was not in a correct format.");
    }
    if (str.length === 0) throw Error("empty string");
    if (str === "NaN" || str === "Infinity" || str === "+Infinity" || str === "-Infinity") return ZERO;
    if (typeof unsigned === "number") {
        // For goog.math.long compatibility
        radix = unsigned, unsigned = false;
    } else {
        unsigned = !!unsigned;
    }
    radix = radix || 10;
    if (radix < 2 || 36 < radix) throw RangeError("radix");
    var p = str.indexOf("-");
    if (p > 0) throw Error("interior hyphen");else if (p === 0) {
        return fromString(str.substring(1), unsigned, radix).neg();
    }
    // Do several (8) digits each time through the loop, so as to
    // minimize the calls to the very expensive emulated div.
    var radixToPower = fromNumber(pow_dbl(radix, 8));
    var result = ZERO;
    for (var i = 0; i < str.length; i += 8) {
        var size = Math.min(8, str.length - i);
        var value = parseInt(str.substring(i, i + size), radix);
        if (size < 8) {
            var power = fromNumber(pow_dbl(radix, size));
            result = result.mul(power).add(fromNumber(value));
        } else {
            result = result.mul(radixToPower);
            result = result.add(fromNumber(value));
        }
    }
    result.unsigned = unsigned;
    return result;
}
/**
 * Converts the specified value to a
 * @param {!Long|number|string|!{low: number, high: number, unsigned: boolean}} val Value
 * @returns {!Long}
 */
function fromValue(val) {
    if (val /* is compatible */ instanceof Long) return val;
    if (typeof val === "number") return fromNumber(val);
    if (typeof val === "string") return fromString(val);
    // Throws for non-objects, converts non-instanceof Long:
    return fromBits(val.low, val.high, val.unsigned);
}
// NOTE: the compiler should inline these constant values below and then remove these variables, so there should be
// no runtime penalty for these.
var TWO_PWR_16_DBL = 1 << 16;
var TWO_PWR_24_DBL = 1 << 24;
var TWO_PWR_32_DBL = TWO_PWR_16_DBL * TWO_PWR_16_DBL;
var TWO_PWR_64_DBL = TWO_PWR_32_DBL * TWO_PWR_32_DBL;
var TWO_PWR_63_DBL = TWO_PWR_64_DBL / 2;
var TWO_PWR_24 = fromInt(TWO_PWR_24_DBL);
/**
 * Signed zero.
 * @type {!Long}
 */
var ZERO = fromInt(0);
/**
 * Unsigned zero.
 * @type {!Long}
 */
var UZERO = fromInt(0, true);
/**
 * Signed one.
 * @type {!Long}
 */
var ONE = fromInt(1);
/**
 * Unsigned one.
 * @type {!Long}
 */
var UONE = fromInt(1, true);
/**
 * Signed negative one.
 * @type {!Long}
 */
var NEG_ONE = fromInt(-1);
/**
 * Maximum signed value.
 * @type {!Long}
 */
var MAX_VALUE = fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0, false);
/**
 * Maximum unsigned value.
 * @type {!Long}
 */
var MAX_UNSIGNED_VALUE = fromBits(0xFFFFFFFF | 0, 0xFFFFFFFF | 0, true);
/**
 * Minimum signed value.
 * @type {!Long}
 */
var MIN_VALUE = fromBits(0, 0x80000000 | 0, false);

/***/ }),
/* 40 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* unused harmony export compare */
/* unused harmony export compareTo */
/* harmony export (immutable) */ __webpack_exports__["d"] = startsWith;
/* unused harmony export indexOfAny */
/* harmony export (immutable) */ __webpack_exports__["a"] = fsFormat;
/* unused harmony export format */
/* unused harmony export endsWith */
/* unused harmony export initialize */
/* unused harmony export insert */
/* unused harmony export isNullOrEmpty */
/* unused harmony export isNullOrWhiteSpace */
/* harmony export (immutable) */ __webpack_exports__["e"] = join;
/* harmony export (immutable) */ __webpack_exports__["f"] = newGuid;
/* unused harmony export padLeft */
/* unused harmony export padRight */
/* unused harmony export remove */
/* harmony export (immutable) */ __webpack_exports__["c"] = replace;
/* unused harmony export replicate */
/* unused harmony export getCharAtIndex */
/* harmony export (immutable) */ __webpack_exports__["b"] = split;
/* unused harmony export trim */
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_array_from__ = __webpack_require__(19);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_array_from___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_array_from__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_get_iterator__ = __webpack_require__(14);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_get_iterator___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_get_iterator__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2__Date__ = __webpack_require__(18);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3__RegExp__ = __webpack_require__(85);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4__Util__ = __webpack_require__(1);










var fsFormatRegExp = /(^|[^%])%([0+ ]*)(-?\d+)?(?:\.(\d+))?(\w)/;
var formatRegExp = /\{(\d+)(,-?\d+)?(?:\:(.+?))?\}/g;
var StringComparison = {
    CurrentCulture: 0,
    CurrentCultureIgnoreCase: 1,
    InvariantCulture: 2,
    InvariantCultureIgnoreCase: 3,
    Ordinal: 4,
    OrdinalIgnoreCase: 5
};
function cmp(x, y, ic) {
    function isIgnoreCase(i) {
        return i === true || i === StringComparison.CurrentCultureIgnoreCase || i === StringComparison.InvariantCultureIgnoreCase || i === StringComparison.OrdinalIgnoreCase;
    }
    function isOrdinal(i) {
        return i === StringComparison.Ordinal || i === StringComparison.OrdinalIgnoreCase;
    }
    if (x == null) {
        return y == null ? 0 : -1;
    }
    if (y == null) {
        return 1;
    } // everything is bigger than null
    if (isOrdinal(ic)) {
        if (isIgnoreCase(ic)) {
            x = x.toLowerCase();
            y = y.toLowerCase();
        }
        return x === y ? 0 : x < y ? -1 : 1;
    } else {
        if (isIgnoreCase(ic)) {
            x = x.toLocaleLowerCase();
            y = y.toLocaleLowerCase();
        }
        return x.localeCompare(y);
    }
}
function compare() {
    for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
        args[_key] = arguments[_key];
    }

    switch (args.length) {
        case 2:
            return cmp(args[0], args[1], false);
        case 3:
            return cmp(args[0], args[1], args[2]);
        case 4:
            return cmp(args[0], args[1], args[2] === true);
        case 5:
            return cmp(args[0].substr(args[1], args[4]), args[2].substr(args[3], args[4]), false);
        case 6:
            return cmp(args[0].substr(args[1], args[4]), args[2].substr(args[3], args[4]), args[5]);
        case 7:
            return cmp(args[0].substr(args[1], args[4]), args[2].substr(args[3], args[4]), args[5] === true);
        default:
            throw new Error("String.compare: Unsupported number of parameters");
    }
}
function compareTo(x, y) {
    return cmp(x, y, false);
}
function startsWith(str, pattern, ic) {
    if (str.length >= pattern.length) {
        return cmp(str.substr(0, pattern.length), pattern, ic) === 0;
    }
    return false;
}
function indexOfAny(str, anyOf) {
    if (str == null || str === "") {
        return -1;
    }
    var startIndex = (arguments.length <= 2 ? 0 : arguments.length - 2) > 0 ? arguments.length <= 2 ? undefined : arguments[2] : 0;
    if (startIndex < 0) {
        throw new Error("String.indexOfAny: Start index cannot be negative");
    }
    var length = (arguments.length <= 2 ? 0 : arguments.length - 2) > 1 ? arguments.length <= 3 ? undefined : arguments[3] : str.length - startIndex;
    if (length < 0) {
        throw new Error("String.indexOfAny: Length cannot be negative");
    }
    if (length > str.length - startIndex) {
        throw new Error("String.indexOfAny: Invalid startIndex and length");
    }
    str = str.substr(startIndex, length);
    var _iteratorNormalCompletion = true;
    var _didIteratorError = false;
    var _iteratorError = undefined;

    try {
        for (var _iterator = __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_get_iterator___default()(anyOf), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
            var c = _step.value;

            var index = str.indexOf(c);
            if (index > -1) {
                return index + startIndex;
            }
        }
    } catch (err) {
        _didIteratorError = true;
        _iteratorError = err;
    } finally {
        try {
            if (!_iteratorNormalCompletion && _iterator.return) {
                _iterator.return();
            }
        } finally {
            if (_didIteratorError) {
                throw _iteratorError;
            }
        }
    }

    return -1;
}
function toHex(value) {
    return value < 0 ? "ff" + (16777215 - (Math.abs(value) - 1)).toString(16) : value.toString(16);
}
function fsFormat(str) {
    function formatOnce(str2, rep) {
        return str2.replace(fsFormatRegExp, function (_, prefix, flags, pad, precision, format) {
            switch (format) {
                case "f":
                case "F":
                    rep = rep.toFixed(precision || 6);
                    break;
                case "g":
                case "G":
                    rep = rep.toPrecision(precision);
                    break;
                case "e":
                case "E":
                    rep = rep.toExponential(precision);
                    break;
                case "O":
                    rep = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__Util__["a" /* toString */])(rep);
                    break;
                case "A":
                    rep = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__Util__["a" /* toString */])(rep, true);
                    break;
                case "x":
                    rep = toHex(Number(rep));
                    break;
                case "X":
                    rep = toHex(Number(rep)).toUpperCase();
                    break;
            }
            var plusPrefix = flags.indexOf("+") >= 0 && parseInt(rep, 10) >= 0;
            pad = parseInt(pad, 10);
            if (!isNaN(pad)) {
                var ch = pad >= 0 && flags.indexOf("0") >= 0 ? "0" : " ";
                rep = padLeft(rep, Math.abs(pad) - (plusPrefix ? 1 : 0), ch, pad < 0);
            }
            var once = prefix + (plusPrefix ? "+" + rep : rep);
            return once.replace(/%/g, "%%");
        });
    }

    for (var _len2 = arguments.length, args = Array(_len2 > 1 ? _len2 - 1 : 0), _key2 = 1; _key2 < _len2; _key2++) {
        args[_key2 - 1] = arguments[_key2];
    }

    if (args.length === 0) {
        return function (cont) {
            if (fsFormatRegExp.test(str)) {
                return function () {
                    for (var _len3 = arguments.length, args2 = Array(_len3), _key3 = 0; _key3 < _len3; _key3++) {
                        args2[_key3] = arguments[_key3];
                    }

                    var strCopy = str;
                    var _iteratorNormalCompletion2 = true;
                    var _didIteratorError2 = false;
                    var _iteratorError2 = undefined;

                    try {
                        for (var _iterator2 = __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_get_iterator___default()(args2), _step2; !(_iteratorNormalCompletion2 = (_step2 = _iterator2.next()).done); _iteratorNormalCompletion2 = true) {
                            var arg = _step2.value;

                            strCopy = formatOnce(strCopy, arg);
                        }
                    } catch (err) {
                        _didIteratorError2 = true;
                        _iteratorError2 = err;
                    } finally {
                        try {
                            if (!_iteratorNormalCompletion2 && _iterator2.return) {
                                _iterator2.return();
                            }
                        } finally {
                            if (_didIteratorError2) {
                                throw _iteratorError2;
                            }
                        }
                    }

                    return cont(strCopy.replace(/%%/g, "%"));
                };
            } else {
                return cont(str);
            }
        };
    } else {
        var _iteratorNormalCompletion3 = true;
        var _didIteratorError3 = false;
        var _iteratorError3 = undefined;

        try {
            for (var _iterator3 = __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_get_iterator___default()(args), _step3; !(_iteratorNormalCompletion3 = (_step3 = _iterator3.next()).done); _iteratorNormalCompletion3 = true) {
                var arg = _step3.value;

                str = formatOnce(str, arg);
            }
        } catch (err) {
            _didIteratorError3 = true;
            _iteratorError3 = err;
        } finally {
            try {
                if (!_iteratorNormalCompletion3 && _iterator3.return) {
                    _iterator3.return();
                }
            } finally {
                if (_didIteratorError3) {
                    throw _iteratorError3;
                }
            }
        }

        return str.replace(/%%/g, "%");
    }
}
function format(str) {
    for (var _len4 = arguments.length, args = Array(_len4 > 1 ? _len4 - 1 : 0), _key4 = 1; _key4 < _len4; _key4++) {
        args[_key4 - 1] = arguments[_key4];
    }

    return str.replace(formatRegExp, function (match, idx, pad, format) {
        var rep = args[idx];
        var padSymbol = " ";
        if (typeof rep === "number") {
            switch ((format || "").substring(0, 1)) {
                case "f":
                case "F":
                    rep = format.length > 1 ? rep.toFixed(format.substring(1)) : rep.toFixed(2);
                    break;
                case "g":
                case "G":
                    rep = format.length > 1 ? rep.toPrecision(format.substring(1)) : rep.toPrecision();
                    break;
                case "e":
                case "E":
                    rep = format.length > 1 ? rep.toExponential(format.substring(1)) : rep.toExponential();
                    break;
                case "p":
                case "P":
                    rep = (format.length > 1 ? (rep * 100).toFixed(format.substring(1)) : (rep * 100).toFixed(2)) + " %";
                    break;
                case "x":
                    rep = toHex(Number(rep));
                    break;
                case "X":
                    rep = toHex(Number(rep)).toUpperCase();
                    break;
                default:
                    var m = /^(0+)(\.0+)?$/.exec(format);
                    if (m != null) {
                        var decs = 0;
                        if (m[2] != null) {
                            rep = rep.toFixed(decs = m[2].length - 1);
                        }
                        pad = "," + (m[1].length + (decs ? decs + 1 : 0)).toString();
                        padSymbol = "0";
                    } else if (format) {
                        rep = format;
                    }
            }
        } else if (rep instanceof Date) {
            if (format.length === 1) {
                switch (format) {
                    case "D":
                        rep = rep.toDateString();
                        break;
                    case "T":
                        rep = rep.toLocaleTimeString();
                        break;
                    case "d":
                        rep = rep.toLocaleDateString();
                        break;
                    case "t":
                        rep = rep.toLocaleTimeString().replace(/:\d\d(?!:)/, "");
                        break;
                    case "o":
                    case "O":
                        if (rep.kind === 2 /* Local */) {
                                var offset = rep.getTimezoneOffset() * -1;
                                rep = format("{0:yyyy-MM-dd}T{0:HH:mm}:{1:00.000}{2}{3:00}:{4:00}", rep, __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Date__["a" /* second */])(rep), offset >= 0 ? "+" : "-", ~~(offset / 60), offset % 60);
                            } else {
                            rep = rep.toISOString();
                        }
                }
            } else {
                rep = format.replace(/\w+/g, function (match2) {
                    var rep2 = match2;
                    switch (match2.substring(0, 1)) {
                        case "y":
                            rep2 = match2.length < 4 ? __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Date__["b" /* year */])(rep) % 100 : __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Date__["b" /* year */])(rep);
                            break;
                        case "h":
                            rep2 = rep.getHours() > 12 ? __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Date__["c" /* hour */])(rep) % 12 : __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Date__["c" /* hour */])(rep);
                            break;
                        case "M":
                            rep2 = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Date__["d" /* month */])(rep);
                            break;
                        case "d":
                            rep2 = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Date__["e" /* day */])(rep);
                            break;
                        case "H":
                            rep2 = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Date__["c" /* hour */])(rep);
                            break;
                        case "m":
                            rep2 = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Date__["f" /* minute */])(rep);
                            break;
                        case "s":
                            rep2 = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Date__["a" /* second */])(rep);
                            break;
                    }
                    if (rep2 !== match2 && rep2 < 10 && match2.length > 1) {
                        rep2 = "0" + rep2;
                    }
                    return rep2;
                });
            }
        }
        pad = parseInt((pad || "").substring(1), 10);
        if (!isNaN(pad)) {
            rep = padLeft(rep, Math.abs(pad), padSymbol, pad < 0);
        }
        return rep;
    });
}
function endsWith(str, search) {
    var idx = str.lastIndexOf(search);
    return idx >= 0 && idx === str.length - search.length;
}
function initialize(n, f) {
    if (n < 0) {
        throw new Error("String length must be non-negative");
    }
    var xs = new Array(n);
    for (var i = 0; i < n; i++) {
        xs[i] = f(i);
    }
    return xs.join("");
}
function insert(str, startIndex, value) {
    if (startIndex < 0 || startIndex > str.length) {
        throw new Error("startIndex is negative or greater than the length of this instance.");
    }
    return str.substring(0, startIndex) + value + str.substring(startIndex);
}
function isNullOrEmpty(str) {
    return typeof str !== "string" || str.length === 0;
}
function isNullOrWhiteSpace(str) {
    return typeof str !== "string" || /^\s*$/.test(str);
}
function join(delimiter, xs) {
    var xs2 = xs;
    if (typeof xs === "string") {
        var len = arguments.length;
        xs2 = Array(len - 1);
        for (var key = 1; key < len; key++) {
            xs2[key - 1] = arguments[key];
        }
    } else if (!Array.isArray(xs)) {
        xs2 = __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_array_from___default()(xs);
    }
    return xs2.join(delimiter);
}
function newGuid() {
    var uuid = "";
    for (var i = 0; i < 32; i++) {
        var random = Math.random() * 16 | 0;
        if (i === 8 || i === 12 || i === 16 || i === 20) {
            uuid += "-";
        }
        uuid += (i === 12 ? 4 : i === 16 ? random & 3 | 8 : random).toString(16);
    }
    return uuid;
}
function padLeft(str, len, ch, isRight) {
    ch = ch || " ";
    str = String(str);
    len = len - str.length;
    for (var i = 0; i < len; i++) {
        str = isRight ? str + ch : ch + str;
    }
    return str;
}
function padRight(str, len, ch) {
    return padLeft(str, len, ch, true);
}
function remove(str, startIndex, count) {
    if (startIndex >= str.length) {
        throw new Error("startIndex must be less than length of string");
    }
    if (typeof count === "number" && startIndex + count > str.length) {
        throw new Error("Index and count must refer to a location within the string.");
    }
    return str.slice(0, startIndex) + (typeof count === "number" ? str.substr(startIndex + count) : "");
}
function replace(str, search, replace) {
    return str.replace(new RegExp(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_3__RegExp__["a" /* escape */])(search), "g"), replace);
}
function replicate(n, x) {
    return initialize(n, function () {
        return x;
    });
}
function getCharAtIndex(input, index) {
    if (index < 0 || index > input.length) {
        throw new Error("System.IndexOutOfRangeException: Index was outside the bounds of the array.");
    }
    return input[index];
}
function split(str, splitters, count, removeEmpty) {
    count = typeof count === "number" ? count : null;
    removeEmpty = typeof removeEmpty === "number" ? removeEmpty : null;
    if (count < 0) {
        throw new Error("Count cannot be less than zero");
    }
    if (count === 0) {
        return [];
    }
    var splitters2 = splitters;
    if (!Array.isArray(splitters)) {
        var len = arguments.length;
        splitters2 = Array(len - 1);
        for (var key = 1; key < len; key++) {
            splitters2[key - 1] = arguments[key];
        }
    }
    splitters2 = splitters2.map(function (x) {
        return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_3__RegExp__["a" /* escape */])(x);
    });
    splitters2 = splitters2.length > 0 ? splitters2 : [" "];
    var i = 0;
    var splits = [];
    var reg = new RegExp(splitters2.join("|"), "g");
    while (count == null || count > 1) {
        var m = reg.exec(str);
        if (m === null) {
            break;
        }
        if (!removeEmpty || m.index - i > 0) {
            count = count != null ? count - 1 : count;
            splits.push(str.substring(i, m.index));
        }
        i = reg.lastIndex;
    }
    if (!removeEmpty || str.length - i > 0) {
        splits.push(str.substring(i));
    }
    return splits;
}
function trim(str, side) {
    for (var _len5 = arguments.length, chars = Array(_len5 > 2 ? _len5 - 2 : 0), _key5 = 2; _key5 < _len5; _key5++) {
        chars[_key5 - 2] = arguments[_key5];
    }

    if (side === "both" && chars.length === 0) {
        return str.trim();
    }
    if (side === "start" || side === "both") {
        var reg = chars.length === 0 ? /^\s+/ : new RegExp("^[" + __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_3__RegExp__["a" /* escape */])(chars.join("")) + "]+");
        str = str.replace(reg, "");
    }
    if (side === "end" || side === "both") {
        var _reg = chars.length === 0 ? /\s+$/ : new RegExp("[" + __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_3__RegExp__["a" /* escape */])(chars.join("")) + "]+$");
        str = str.replace(_reg, "");
    }
    return str;
}

/***/ }),
/* 41 */
/***/ (function(module, exports, __webpack_require__) {

// getting tag from 19.1.3.6 Object.prototype.toString()
var cof = __webpack_require__(42)
  , TAG = __webpack_require__(2)('toStringTag')
  // ES3 wrong here
  , ARG = cof(function(){ return arguments; }()) == 'Arguments';

// fallback for IE11 Script Access Denied error
var tryGet = function(it, key){
  try {
    return it[key];
  } catch(e){ /* empty */ }
};

module.exports = function(it){
  var O, T, B;
  return it === undefined ? 'Undefined' : it === null ? 'Null'
    // @@toStringTag case
    : typeof (T = tryGet(O = Object(it), TAG)) == 'string' ? T
    // builtinTag case
    : ARG ? cof(O)
    // ES3 arguments fallback
    : (B = cof(O)) == 'Object' && typeof O.callee == 'function' ? 'Arguments' : B;
};

/***/ }),
/* 42 */
/***/ (function(module, exports) {

var toString = {}.toString;

module.exports = function(it){
  return toString.call(it).slice(8, -1);
};

/***/ }),
/* 43 */
/***/ (function(module, exports) {

// IE 8- don't enum bug keys
module.exports = (
  'constructor,hasOwnProperty,isPrototypeOf,propertyIsEnumerable,toLocaleString,toString,valueOf'
).split(',');

/***/ }),
/* 44 */
/***/ (function(module, exports, __webpack_require__) {

var ctx         = __webpack_require__(20)
  , call        = __webpack_require__(73)
  , isArrayIter = __webpack_require__(71)
  , anObject    = __webpack_require__(11)
  , toLength    = __webpack_require__(35)
  , getIterFn   = __webpack_require__(58)
  , BREAK       = {}
  , RETURN      = {};
var exports = module.exports = function(iterable, entries, fn, that, ITERATOR){
  var iterFn = ITERATOR ? function(){ return iterable; } : getIterFn(iterable)
    , f      = ctx(fn, that, entries ? 2 : 1)
    , index  = 0
    , length, step, iterator, result;
  if(typeof iterFn != 'function')throw TypeError(iterable + ' is not iterable!');
  // fast case for arrays with default iterator
  if(isArrayIter(iterFn))for(length = toLength(iterable.length); length > index; index++){
    result = entries ? f(anObject(step = iterable[index])[0], step[1]) : f(iterable[index]);
    if(result === BREAK || result === RETURN)return result;
  } else for(iterator = iterFn.call(iterable); !(step = iterator.next()).done; ){
    result = call(iterator, f, step.value, entries);
    if(result === BREAK || result === RETURN)return result;
  }
};
exports.BREAK  = BREAK;
exports.RETURN = RETURN;

/***/ }),
/* 45 */
/***/ (function(module, exports, __webpack_require__) {

// fallback for non-array-like ES3 and non-enumerable old V8 strings
var cof = __webpack_require__(42);
module.exports = Object('z').propertyIsEnumerable(0) ? Object : function(it){
  return cof(it) == 'String' ? it.split('') : Object(it);
};

/***/ }),
/* 46 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

var LIBRARY        = __webpack_require__(47)
  , $export        = __webpack_require__(4)
  , redefine       = __webpack_require__(80)
  , hide           = __webpack_require__(12)
  , has            = __webpack_require__(16)
  , Iterators      = __webpack_require__(21)
  , $iterCreate    = __webpack_require__(124)
  , setToStringTag = __webpack_require__(34)
  , getPrototypeOf = __webpack_require__(77)
  , ITERATOR       = __webpack_require__(2)('iterator')
  , BUGGY          = !([].keys && 'next' in [].keys()) // Safari has buggy iterators w/o `next`
  , FF_ITERATOR    = '@@iterator'
  , KEYS           = 'keys'
  , VALUES         = 'values';

var returnThis = function(){ return this; };

module.exports = function(Base, NAME, Constructor, next, DEFAULT, IS_SET, FORCED){
  $iterCreate(Constructor, NAME, next);
  var getMethod = function(kind){
    if(!BUGGY && kind in proto)return proto[kind];
    switch(kind){
      case KEYS: return function keys(){ return new Constructor(this, kind); };
      case VALUES: return function values(){ return new Constructor(this, kind); };
    } return function entries(){ return new Constructor(this, kind); };
  };
  var TAG        = NAME + ' Iterator'
    , DEF_VALUES = DEFAULT == VALUES
    , VALUES_BUG = false
    , proto      = Base.prototype
    , $native    = proto[ITERATOR] || proto[FF_ITERATOR] || DEFAULT && proto[DEFAULT]
    , $default   = $native || getMethod(DEFAULT)
    , $entries   = DEFAULT ? !DEF_VALUES ? $default : getMethod('entries') : undefined
    , $anyNative = NAME == 'Array' ? proto.entries || $native : $native
    , methods, key, IteratorPrototype;
  // Fix native
  if($anyNative){
    IteratorPrototype = getPrototypeOf($anyNative.call(new Base));
    if(IteratorPrototype !== Object.prototype){
      // Set @@toStringTag to native iterators
      setToStringTag(IteratorPrototype, TAG, true);
      // fix for some old engines
      if(!LIBRARY && !has(IteratorPrototype, ITERATOR))hide(IteratorPrototype, ITERATOR, returnThis);
    }
  }
  // fix Array#{values, @@iterator}.name in V8 / FF
  if(DEF_VALUES && $native && $native.name !== VALUES){
    VALUES_BUG = true;
    $default = function values(){ return $native.call(this); };
  }
  // Define iterator
  if((!LIBRARY || FORCED) && (BUGGY || VALUES_BUG || !proto[ITERATOR])){
    hide(proto, ITERATOR, $default);
  }
  // Plug for library
  Iterators[NAME] = $default;
  Iterators[TAG]  = returnThis;
  if(DEFAULT){
    methods = {
      values:  DEF_VALUES ? $default : getMethod(VALUES),
      keys:    IS_SET     ? $default : getMethod(KEYS),
      entries: $entries
    };
    if(FORCED)for(key in methods){
      if(!(key in proto))redefine(proto, key, methods[key]);
    } else $export($export.P + $export.F * (BUGGY || VALUES_BUG), NAME, methods);
  }
  return methods;
};

/***/ }),
/* 47 */
/***/ (function(module, exports) {

module.exports = true;

/***/ }),
/* 48 */
/***/ (function(module, exports, __webpack_require__) {

var META     = __webpack_require__(36)('meta')
  , isObject = __webpack_require__(17)
  , has      = __webpack_require__(16)
  , setDesc  = __webpack_require__(5).f
  , id       = 0;
var isExtensible = Object.isExtensible || function(){
  return true;
};
var FREEZE = !__webpack_require__(15)(function(){
  return isExtensible(Object.preventExtensions({}));
});
var setMeta = function(it){
  setDesc(it, META, {value: {
    i: 'O' + ++id, // object ID
    w: {}          // weak collections IDs
  }});
};
var fastKey = function(it, create){
  // return primitive with prefix
  if(!isObject(it))return typeof it == 'symbol' ? it : (typeof it == 'string' ? 'S' : 'P') + it;
  if(!has(it, META)){
    // can't set metadata to uncaught frozen object
    if(!isExtensible(it))return 'F';
    // not necessary to add metadata
    if(!create)return 'E';
    // add missing metadata
    setMeta(it);
  // return object ID
  } return it[META].i;
};
var getWeak = function(it, create){
  if(!has(it, META)){
    // can't set metadata to uncaught frozen object
    if(!isExtensible(it))return true;
    // not necessary to add metadata
    if(!create)return false;
    // add missing metadata
    setMeta(it);
  // return hash weak collections IDs
  } return it[META].w;
};
// add metadata on freeze-family methods calling
var onFreeze = function(it){
  if(FREEZE && meta.NEED && isExtensible(it) && !has(it, META))setMeta(it);
  return it;
};
var meta = module.exports = {
  KEY:      META,
  NEED:     false,
  fastKey:  fastKey,
  getWeak:  getWeak,
  onFreeze: onFreeze
};

/***/ }),
/* 49 */
/***/ (function(module, exports, __webpack_require__) {

var pIE            = __webpack_require__(33)
  , createDesc     = __webpack_require__(25)
  , toIObject      = __webpack_require__(13)
  , toPrimitive    = __webpack_require__(55)
  , has            = __webpack_require__(16)
  , IE8_DOM_DEFINE = __webpack_require__(70)
  , gOPD           = Object.getOwnPropertyDescriptor;

exports.f = __webpack_require__(7) ? gOPD : function getOwnPropertyDescriptor(O, P){
  O = toIObject(O);
  P = toPrimitive(P, true);
  if(IE8_DOM_DEFINE)try {
    return gOPD(O, P);
  } catch(e){ /* empty */ }
  if(has(O, P))return createDesc(!pIE.f.call(O, P), O[P]);
};

/***/ }),
/* 50 */
/***/ (function(module, exports) {

exports.f = Object.getOwnPropertySymbols;

/***/ }),
/* 51 */
/***/ (function(module, exports, __webpack_require__) {

// most Object methods by ES6 should accept primitives
var $export = __webpack_require__(4)
  , core    = __webpack_require__(0)
  , fails   = __webpack_require__(15);
module.exports = function(KEY, exec){
  var fn  = (core.Object || {})[KEY] || Object[KEY]
    , exp = {};
  exp[KEY] = exec(fn);
  $export($export.S + $export.F * fails(function(){ fn(1); }), 'Object', exp);
};

/***/ }),
/* 52 */
/***/ (function(module, exports, __webpack_require__) {

var shared = __webpack_require__(53)('keys')
  , uid    = __webpack_require__(36);
module.exports = function(key){
  return shared[key] || (shared[key] = uid(key));
};

/***/ }),
/* 53 */
/***/ (function(module, exports, __webpack_require__) {

var global = __webpack_require__(8)
  , SHARED = '__core-js_shared__'
  , store  = global[SHARED] || (global[SHARED] = {});
module.exports = function(key){
  return store[key] || (store[key] = {});
};

/***/ }),
/* 54 */
/***/ (function(module, exports) {

// 7.1.4 ToInteger
var ceil  = Math.ceil
  , floor = Math.floor;
module.exports = function(it){
  return isNaN(it = +it) ? 0 : (it > 0 ? floor : ceil)(it);
};

/***/ }),
/* 55 */
/***/ (function(module, exports, __webpack_require__) {

// 7.1.1 ToPrimitive(input [, PreferredType])
var isObject = __webpack_require__(17);
// instead of the ES6 spec version, we didn't implement @@toPrimitive case
// and the second argument - flag - preferred type is a string
module.exports = function(it, S){
  if(!isObject(it))return it;
  var fn, val;
  if(S && typeof (fn = it.toString) == 'function' && !isObject(val = fn.call(it)))return val;
  if(typeof (fn = it.valueOf) == 'function' && !isObject(val = fn.call(it)))return val;
  if(!S && typeof (fn = it.toString) == 'function' && !isObject(val = fn.call(it)))return val;
  throw TypeError("Can't convert object to primitive value");
};

/***/ }),
/* 56 */
/***/ (function(module, exports, __webpack_require__) {

var global         = __webpack_require__(8)
  , core           = __webpack_require__(0)
  , LIBRARY        = __webpack_require__(47)
  , wksExt         = __webpack_require__(57)
  , defineProperty = __webpack_require__(5).f;
module.exports = function(name){
  var $Symbol = core.Symbol || (core.Symbol = LIBRARY ? {} : global.Symbol || {});
  if(name.charAt(0) != '_' && !(name in $Symbol))defineProperty($Symbol, name, {value: wksExt.f(name)});
};

/***/ }),
/* 57 */
/***/ (function(module, exports, __webpack_require__) {

exports.f = __webpack_require__(2);

/***/ }),
/* 58 */
/***/ (function(module, exports, __webpack_require__) {

var classof   = __webpack_require__(41)
  , ITERATOR  = __webpack_require__(2)('iterator')
  , Iterators = __webpack_require__(21);
module.exports = __webpack_require__(0).getIteratorMethod = function(it){
  if(it != undefined)return it[ITERATOR]
    || it['@@iterator']
    || Iterators[classof(it)];
};

/***/ }),
/* 59 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* unused harmony export addRangeInPlace */
/* unused harmony export copyTo */
/* unused harmony export partition */
/* harmony export (immutable) */ __webpack_exports__["b"] = permute;
/* unused harmony export removeInPlace */
/* unused harmony export setSlice */
/* unused harmony export sortInPlaceBy */
/* unused harmony export unzip */
/* unused harmony export unzip3 */
/* harmony export (immutable) */ __webpack_exports__["a"] = chunkBySize;
/* unused harmony export getSubArray */
/* unused harmony export fill */
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_get_iterator__ = __webpack_require__(14);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_get_iterator___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_get_iterator__);

function addRangeInPlace(range, xs) {
    var iter = __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_get_iterator___default()(range);
    var cur = iter.next();
    while (!cur.done) {
        xs.push(cur.value);
        cur = iter.next();
    }
}
function copyTo(source, sourceIndex, target, targetIndex, count) {
    while (count--) {
        target[targetIndex++] = source[sourceIndex++];
    }
}
function partition(f, xs) {
    var ys = [];
    var zs = [];
    var j = 0;
    var k = 0;
    for (var i = 0; i < xs.length; i++) {
        if (f(xs[i])) {
            ys[j++] = xs[i];
        } else {
            zs[k++] = xs[i];
        }
    }
    return [ys, zs];
}
function permute(f, xs) {
    // Keep the type of the array
    var ys = xs.map(function () {
        return null;
    });
    var checkFlags = new Array(xs.length);
    for (var i = 0; i < xs.length; i++) {
        var j = f(i);
        if (j < 0 || j >= xs.length) {
            throw new Error("Not a valid permutation");
        }
        ys[j] = xs[i];
        checkFlags[j] = 1;
    }
    for (var _i = 0; _i < xs.length; _i++) {
        if (checkFlags[_i] !== 1) {
            throw new Error("Not a valid permutation");
        }
    }
    return ys;
}
function removeInPlace(item, xs) {
    var i = xs.indexOf(item);
    if (i > -1) {
        xs.splice(i, 1);
        return true;
    }
    return false;
}
function setSlice(target, lower, upper, source) {
    var length = (upper || target.length - 1) - lower;
    if (ArrayBuffer.isView(target) && source.length <= length) {
        target.set(source, lower);
    } else {
        for (var i = lower | 0, j = 0; j <= length; i++, j++) {
            target[i] = source[j];
        }
    }
}
function sortInPlaceBy(f, xs) {
    var dir = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 1;

    return xs.sort(function (x, y) {
        x = f(x);
        y = f(y);
        return (x < y ? -1 : x === y ? 0 : 1) * dir;
    });
}
function unzip(xs) {
    var bs = new Array(xs.length);
    var cs = new Array(xs.length);
    for (var i = 0; i < xs.length; i++) {
        bs[i] = xs[i][0];
        cs[i] = xs[i][1];
    }
    return [bs, cs];
}
function unzip3(xs) {
    var bs = new Array(xs.length);
    var cs = new Array(xs.length);
    var ds = new Array(xs.length);
    for (var i = 0; i < xs.length; i++) {
        bs[i] = xs[i][0];
        cs[i] = xs[i][1];
        ds[i] = xs[i][2];
    }
    return [bs, cs, ds];
}
function chunkBySize(size, xs) {
    if (size < 1) {
        throw new Error("The input must be positive. parameter name: chunkSize");
    }
    if (xs.length === 0) {
        return [[]];
    }
    var result = [];
    // add each chunk to the result
    for (var x = 0; x < Math.ceil(xs.length / size); x++) {
        var start = x * size;
        var end = start + size;
        result.push(xs.slice(start, end));
    }
    return result;
}
function getSubArray(xs, startIndex, count) {
    return xs.slice(startIndex, startIndex + count);
}
function fill(target, targetIndex, count, value) {
    target.fill(value, targetIndex, targetIndex + count);
}

/***/ }),
/* 60 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* unused harmony export map */
/* unused harmony export mapError */
/* harmony export (immutable) */ __webpack_exports__["b"] = bind;
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_classCallCheck__ = __webpack_require__(9);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_classCallCheck___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_classCallCheck__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_createClass__ = __webpack_require__(10);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_createClass___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_createClass__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2__Symbol__ = __webpack_require__(6);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3__Util__ = __webpack_require__(1);






var Result = function () {
    function Result(tag, data) {
        __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_classCallCheck___default()(this, Result);

        this.tag = tag | 0;
        this.data = data;
    }

    __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_createClass___default()(Result, [{
        key: "Equals",
        value: function Equals(other) {
            return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_3__Util__["g" /* equalsUnions */])(this, other);
        }
    }, {
        key: "CompareTo",
        value: function CompareTo(other) {
            return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_3__Util__["d" /* compareUnions */])(this, other);
        }
    }, {
        key: __WEBPACK_IMPORTED_MODULE_2__Symbol__["a" /* default */].reflection,
        value: function value() {
            return {
                type: "Microsoft.FSharp.Core.FSharpResult",
                interfaces: ["FSharpUnion", "System.IEquatable", "System.IComparable"],
                cases: [["Ok", __WEBPACK_IMPORTED_MODULE_3__Util__["h" /* Any */]], ["Error", __WEBPACK_IMPORTED_MODULE_3__Util__["h" /* Any */]]]
            };
        }
    }]);

    return Result;
}();

/* harmony default export */ __webpack_exports__["a"] = (Result);

function map(f, result) {
    return result.tag === 0 ? new Result(0, f(result.data)) : result;
}
function mapError(f, result) {
    return result.tag === 1 ? new Result(1, f(result.data)) : result;
}
function bind(f, result) {
    return result.tag === 0 ? f(result.data) : result;
}

/***/ }),
/* 61 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(100), __esModule: true };

/***/ }),
/* 62 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(101), __esModule: true };

/***/ }),
/* 63 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(104), __esModule: true };

/***/ }),
/* 64 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(107), __esModule: true };

/***/ }),
/* 65 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(108), __esModule: true };

/***/ }),
/* 66 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(109), __esModule: true };

/***/ }),
/* 67 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


exports.__esModule = true;

var _defineProperty = __webpack_require__(63);

var _defineProperty2 = _interopRequireDefault(_defineProperty);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

exports.default = function (obj, key, value) {
  if (key in obj) {
    (0, _defineProperty2.default)(obj, key, {
      value: value,
      enumerable: true,
      configurable: true,
      writable: true
    });
  } else {
    obj[key] = value;
  }

  return obj;
};

/***/ }),
/* 68 */
/***/ (function(module, exports) {

module.exports = function(it, Constructor, name, forbiddenField){
  if(!(it instanceof Constructor) || (forbiddenField !== undefined && forbiddenField in it)){
    throw TypeError(name + ': incorrect invocation!');
  } return it;
};

/***/ }),
/* 69 */
/***/ (function(module, exports, __webpack_require__) {

var isObject = __webpack_require__(17)
  , document = __webpack_require__(8).document
  // in old IE typeof document.createElement is 'object'
  , is = isObject(document) && isObject(document.createElement);
module.exports = function(it){
  return is ? document.createElement(it) : {};
};

/***/ }),
/* 70 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = !__webpack_require__(7) && !__webpack_require__(15)(function(){
  return Object.defineProperty(__webpack_require__(69)('div'), 'a', {get: function(){ return 7; }}).a != 7;
});

/***/ }),
/* 71 */
/***/ (function(module, exports, __webpack_require__) {

// check on default Array iterator
var Iterators  = __webpack_require__(21)
  , ITERATOR   = __webpack_require__(2)('iterator')
  , ArrayProto = Array.prototype;

module.exports = function(it){
  return it !== undefined && (Iterators.Array === it || ArrayProto[ITERATOR] === it);
};

/***/ }),
/* 72 */
/***/ (function(module, exports, __webpack_require__) {

// 7.2.2 IsArray(argument)
var cof = __webpack_require__(42);
module.exports = Array.isArray || function isArray(arg){
  return cof(arg) == 'Array';
};

/***/ }),
/* 73 */
/***/ (function(module, exports, __webpack_require__) {

// call something on iterator step with safe closing on error
var anObject = __webpack_require__(11);
module.exports = function(iterator, fn, value, entries){
  try {
    return entries ? fn(anObject(value)[0], value[1]) : fn(value);
  // 7.4.6 IteratorClose(iterator, completion)
  } catch(e){
    var ret = iterator['return'];
    if(ret !== undefined)anObject(ret.call(iterator));
    throw e;
  }
};

/***/ }),
/* 74 */
/***/ (function(module, exports) {

module.exports = function(done, value){
  return {value: value, done: !!done};
};

/***/ }),
/* 75 */
/***/ (function(module, exports, __webpack_require__) {

// fallback for IE11 buggy Object.getOwnPropertyNames with iframe and window
var toIObject = __webpack_require__(13)
  , gOPN      = __webpack_require__(76).f
  , toString  = {}.toString;

var windowNames = typeof window == 'object' && window && Object.getOwnPropertyNames
  ? Object.getOwnPropertyNames(window) : [];

var getWindowNames = function(it){
  try {
    return gOPN(it);
  } catch(e){
    return windowNames.slice();
  }
};

module.exports.f = function getOwnPropertyNames(it){
  return windowNames && toString.call(it) == '[object Window]' ? getWindowNames(it) : gOPN(toIObject(it));
};


/***/ }),
/* 76 */
/***/ (function(module, exports, __webpack_require__) {

// 19.1.2.7 / 15.2.3.4 Object.getOwnPropertyNames(O)
var $keys      = __webpack_require__(78)
  , hiddenKeys = __webpack_require__(43).concat('length', 'prototype');

exports.f = Object.getOwnPropertyNames || function getOwnPropertyNames(O){
  return $keys(O, hiddenKeys);
};

/***/ }),
/* 77 */
/***/ (function(module, exports, __webpack_require__) {

// 19.1.2.9 / 15.2.3.2 Object.getPrototypeOf(O)
var has         = __webpack_require__(16)
  , toObject    = __webpack_require__(26)
  , IE_PROTO    = __webpack_require__(52)('IE_PROTO')
  , ObjectProto = Object.prototype;

module.exports = Object.getPrototypeOf || function(O){
  O = toObject(O);
  if(has(O, IE_PROTO))return O[IE_PROTO];
  if(typeof O.constructor == 'function' && O instanceof O.constructor){
    return O.constructor.prototype;
  } return O instanceof Object ? ObjectProto : null;
};

/***/ }),
/* 78 */
/***/ (function(module, exports, __webpack_require__) {

var has          = __webpack_require__(16)
  , toIObject    = __webpack_require__(13)
  , arrayIndexOf = __webpack_require__(114)(false)
  , IE_PROTO     = __webpack_require__(52)('IE_PROTO');

module.exports = function(object, names){
  var O      = toIObject(object)
    , i      = 0
    , result = []
    , key;
  for(key in O)if(key != IE_PROTO)has(O, key) && result.push(key);
  // Don't enum bug & hidden keys
  while(names.length > i)if(has(O, key = names[i++])){
    ~arrayIndexOf(result, key) || result.push(key);
  }
  return result;
};

/***/ }),
/* 79 */
/***/ (function(module, exports, __webpack_require__) {

var hide = __webpack_require__(12);
module.exports = function(target, src, safe){
  for(var key in src){
    if(safe && target[key])target[key] = src[key];
    else hide(target, key, src[key]);
  } return target;
};

/***/ }),
/* 80 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = __webpack_require__(12);

/***/ }),
/* 81 */
/***/ (function(module, exports) {



/***/ }),
/* 82 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony export (immutable) */ __webpack_exports__["a"] = init;
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0__packages_Fable_Core_fable_core_String__ = __webpack_require__(40);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1__parser_fs__ = __webpack_require__(152);


function init() {
    var doc = document;
    var button1 = doc.getElementsByTagName("button")[0];
    var textbox1 = doc.getElementsByTagName("textarea")[0];
    var div1 = doc.getElementsByTagName("div")[0];
    button1.addEventListener("click", function (_arg1) {
        try {
            var res = void 0;
            var matchValue = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_1__parser_fs__["a" /* run */])(textbox1.value);

            if (matchValue.tag === 1) {
                res = matchValue.data;
            } else {
                var v = matchValue.data[0];
                res = {
                    formatFn: __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_0__packages_Fable_Core_fable_core_String__["a" /* fsFormat */])("%A"),
                    input: "%A"
                }.formatFn(function (x) {
                    return x;
                })(v);
            }

            div1.innerHTML = res;
        } catch (e) {
            div1.innerHTML = {
                formatFn: __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_0__packages_Fable_Core_fable_core_String__["a" /* fsFormat */])("error = %A"),
                input: "error = %A"
            }.formatFn(function (x) {
                return x;
            })(e.message);
        }

        return null;
    });
}
init();

/***/ }),
/* 83 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* unused harmony export tryParse */
/* harmony export (immutable) */ __webpack_exports__["a"] = parse;
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_number_is_nan__ = __webpack_require__(62);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_number_is_nan___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_number_is_nan__);

// TODO does this perfectly match the .NET behavior ?
// TODO does this perfectly match the .NET behavior ?
function tryParse(s, radix, initial) {
    if (s != null) {
        if (radix === 10) {
            var v = +s;
            if (!__WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_number_is_nan___default()(v)) {
                return [true, v];
            }
        }
    }
    return [false, initial];
}
function parse(s) {
    var radix = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 10;

    var a = tryParse(s, radix, 0);
    if (a[0]) {
        return a[1];
    } else {
        // TODO FormatException ?
        throw new Error("Input string was not in a correct format.");
    }
}

/***/ }),
/* 84 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony export (immutable) */ __webpack_exports__["a"] = isValid;
/* unused harmony export tryParse */
/* unused harmony export parse */
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_number_is_nan__ = __webpack_require__(62);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_number_is_nan___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_number_is_nan__);

// TODO verify that this matches the behavior of .NET
var parseRadix10 = /^ *([\+\-]?[0-9]+) *$/;
// TODO verify that this matches the behavior of .NET
var parseRadix16 = /^ *([\+\-]?[0-9a-fA-F]+) *$/;
function isValid(s, radix) {
    if (s != null) {
        if (radix === 16) {
            return parseRadix16.exec(s);
        } else if (radix <= 10) {
            return parseRadix10.exec(s);
        }
    }
    return null;
}
// TODO does this perfectly match the .NET behavior ?
function tryParse(s, radix, initial) {
    var a = isValid(s, radix);
    if (a !== null) {
        var v = parseInt(a[1], radix);
        if (!__WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_number_is_nan___default()(v)) {
            return [true, v];
        }
    }
    return [false, initial];
}
function parse(s) {
    var radix = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 10;

    var a = tryParse(s, radix, 0);
    if (a[0]) {
        return a[1];
    } else {
        // TODO FormatException ?
        throw new Error("Input string was not in a correct format.");
    }
}

/***/ }),
/* 85 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* unused harmony export create */
/* harmony export (immutable) */ __webpack_exports__["a"] = escape;
/* unused harmony export unescape */
/* unused harmony export isMatch */
/* unused harmony export match */
/* unused harmony export matches */
/* unused harmony export options */
/* unused harmony export replace */
/* unused harmony export split */
function create(pattern, options) {
    var flags = "g";
    flags += options & 1 ? "i" : "";
    flags += options & 2 ? "m" : "";
    return new RegExp(pattern, flags);
}
// From http://stackoverflow.com/questions/3446170/escape-string-for-use-in-javascript-regex
function escape(str) {
    return str.replace(/[\-\[\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, "\\$&");
}
function unescape(str) {
    return str.replace(/\\([\-\[\/\{\}\(\)\*\+\?\.\\\^\$\|])/g, "$1");
}
function isMatch(str, pattern) {
    var options = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 0;

    var reg = void 0;
    reg = str instanceof RegExp ? (reg = str, str = pattern, reg.lastIndex = options, reg) : reg = create(pattern, options);
    return reg.test(str);
}
function match(str, pattern) {
    var options = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 0;

    var reg = void 0;
    reg = str instanceof RegExp ? (reg = str, str = pattern, reg.lastIndex = options, reg) : reg = create(pattern, options);
    return reg.exec(str);
}
function matches(str, pattern) {
    var options = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 0;

    var reg = void 0;
    reg = str instanceof RegExp ? (reg = str, str = pattern, reg.lastIndex = options, reg) : reg = create(pattern, options);
    if (!reg.global) {
        throw new Error("Non-global RegExp"); // Prevent infinite loop
    }
    var m = reg.exec(str);
    var matches = [];
    while (m !== null) {
        matches.push(m);
        m = reg.exec(str);
    }
    return matches;
}
function options(reg) {
    var options = 256; // ECMAScript
    options |= reg.ignoreCase ? 1 : 0;
    options |= reg.multiline ? 2 : 0;
    return options;
}
function replace(reg, input, replacement, limit) {
    var offset = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : 0;

    function replacer() {
        var res = arguments[0];
        if (limit !== 0) {
            limit--;
            var _match = [];
            var len = arguments.length;
            for (var i = 0; i < len - 2; i++) {
                _match.push(arguments[i]);
            }
            _match.index = arguments[len - 2];
            _match.input = arguments[len - 1];
            res = replacement(_match);
        }
        return res;
    }
    if (typeof reg === "string") {
        var tmp = reg;
        reg = create(input, limit);
        input = tmp;
        limit = undefined;
    }
    if (typeof replacement === "function") {
        limit = limit == null ? -1 : limit;
        return input.substring(0, offset) + input.substring(offset).replace(reg, replacer);
    } else {
        if (limit != null) {
            var m = void 0;
            var sub1 = input.substring(offset);
            var _matches = matches(reg, sub1);
            var sub2 = matches.length > limit ? (m = _matches[limit - 1], sub1.substring(0, m.index + m[0].length)) : sub1;
            return input.substring(0, offset) + sub2.replace(reg, replacement) + input.substring(offset + sub2.length);
        } else {
            return input.replace(reg, replacement);
        }
    }
}
function split(reg, input, limit) {
    var offset = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : 0;

    if (typeof reg === "string") {
        var tmp = reg;
        reg = create(input, limit);
        input = tmp;
        limit = undefined;
    }
    input = input.substring(offset);
    return input.split(reg, limit);
}

/***/ }),
/* 86 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony export (immutable) */ __webpack_exports__["a"] = create;
/* unused harmony export fromTicks */
/* unused harmony export fromDays */
/* unused harmony export fromHours */
/* unused harmony export fromMinutes */
/* unused harmony export fromSeconds */
/* unused harmony export days */
/* unused harmony export hours */
/* unused harmony export minutes */
/* unused harmony export seconds */
/* unused harmony export milliseconds */
/* unused harmony export ticks */
/* unused harmony export totalDays */
/* unused harmony export totalHours */
/* unused harmony export totalMinutes */
/* unused harmony export totalSeconds */
/* unused harmony export negate */
/* unused harmony export add */
/* unused harmony export subtract */
/* unused harmony export compare */
/* unused harmony export compareTo */
/* unused harmony export duration */
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0__Long__ = __webpack_require__(39);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1__Util__ = __webpack_require__(1);


function create() {
    var d = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 0;
    var h = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 0;
    var m = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 0;
    var s = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : 0;
    var ms = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : 0;

    switch (arguments.length) {
        case 1:
            // ticks
            return fromTicks(arguments[0]);
        case 3:
            // h,m,s
            d = 0, h = arguments[0], m = arguments[1], s = arguments[2], ms = 0;
            break;
        default:
            // d,h,m,s,ms
            break;
    }
    return d * 86400000 + h * 3600000 + m * 60000 + s * 1000 + ms;
}
function fromTicks(ticks) {
    return ticks.div(10000).toNumber();
}
function fromDays(d) {
    return create(d, 0, 0, 0);
}
function fromHours(h) {
    return create(h, 0, 0);
}
function fromMinutes(m) {
    return create(0, m, 0);
}
function fromSeconds(s) {
    return create(0, 0, s);
}
function days(ts) {
    return Math.floor(ts / 86400000);
}
function hours(ts) {
    return Math.floor(ts % 86400000 / 3600000);
}
function minutes(ts) {
    return Math.floor(ts % 3600000 / 60000);
}
function seconds(ts) {
    return Math.floor(ts % 60000 / 1000);
}
function milliseconds(ts) {
    return Math.floor(ts % 1000);
}
function ticks(ts) {
    return __WEBPACK_IMPORTED_MODULE_0__Long__["a" /* fromNumber */](ts).mul(10000);
}
function totalDays(ts) {
    return ts / 86400000;
}
function totalHours(ts) {
    return ts / 3600000;
}
function totalMinutes(ts) {
    return ts / 60000;
}
function totalSeconds(ts) {
    return ts / 1000;
}
function negate(ts) {
    return ts * -1;
}
function add(ts1, ts2) {
    return ts1 + ts2;
}
function subtract(ts1, ts2) {
    return ts1 - ts2;
}
function compare(x, y) {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_1__Util__["b" /* compare */])(x, y);
}
function compareTo(x, y) {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_1__Util__["b" /* compare */])(x, y);
}
function duration(x) {
    return Math.abs(x);
}

/***/ }),
/* 87 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(98), __esModule: true };

/***/ }),
/* 88 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(99), __esModule: true };

/***/ }),
/* 89 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(102), __esModule: true };

/***/ }),
/* 90 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(103), __esModule: true };

/***/ }),
/* 91 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(105), __esModule: true };

/***/ }),
/* 92 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(106), __esModule: true };

/***/ }),
/* 93 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


exports.__esModule = true;

var _setPrototypeOf = __webpack_require__(65);

var _setPrototypeOf2 = _interopRequireDefault(_setPrototypeOf);

var _create = __webpack_require__(90);

var _create2 = _interopRequireDefault(_create);

var _typeof2 = __webpack_require__(30);

var _typeof3 = _interopRequireDefault(_typeof2);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

exports.default = function (subClass, superClass) {
  if (typeof superClass !== "function" && superClass !== null) {
    throw new TypeError("Super expression must either be null or a function, not " + (typeof superClass === "undefined" ? "undefined" : (0, _typeof3.default)(superClass)));
  }

  subClass.prototype = (0, _create2.default)(superClass && superClass.prototype, {
    constructor: {
      value: subClass,
      enumerable: false,
      writable: true,
      configurable: true
    }
  });
  if (superClass) _setPrototypeOf2.default ? (0, _setPrototypeOf2.default)(subClass, superClass) : subClass.__proto__ = superClass;
};

/***/ }),
/* 94 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


exports.__esModule = true;

var _typeof2 = __webpack_require__(30);

var _typeof3 = _interopRequireDefault(_typeof2);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

exports.default = function (self, call) {
  if (!self) {
    throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
  }

  return call && ((typeof call === "undefined" ? "undefined" : (0, _typeof3.default)(call)) === "object" || typeof call === "function") ? call : self;
};

/***/ }),
/* 95 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


exports.__esModule = true;

var _isIterable2 = __webpack_require__(87);

var _isIterable3 = _interopRequireDefault(_isIterable2);

var _getIterator2 = __webpack_require__(14);

var _getIterator3 = _interopRequireDefault(_getIterator2);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

exports.default = function () {
  function sliceIterator(arr, i) {
    var _arr = [];
    var _n = true;
    var _d = false;
    var _e = undefined;

    try {
      for (var _i = (0, _getIterator3.default)(arr), _s; !(_n = (_s = _i.next()).done); _n = true) {
        _arr.push(_s.value);

        if (i && _arr.length === i) break;
      }
    } catch (err) {
      _d = true;
      _e = err;
    } finally {
      try {
        if (!_n && _i["return"]) _i["return"]();
      } finally {
        if (_d) throw _e;
      }
    }

    return _arr;
  }

  return function (arr, i) {
    if (Array.isArray(arr)) {
      return arr;
    } else if ((0, _isIterable3.default)(Object(arr))) {
      return sliceIterator(arr, i);
    } else {
      throw new TypeError("Invalid attempt to destructure non-iterable instance");
    }
  };
}();

/***/ }),
/* 96 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(27);
__webpack_require__(135);
module.exports = __webpack_require__(0).Array.from;

/***/ }),
/* 97 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(37);
__webpack_require__(27);
module.exports = __webpack_require__(133);

/***/ }),
/* 98 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(37);
__webpack_require__(27);
module.exports = __webpack_require__(134);

/***/ }),
/* 99 */
/***/ (function(module, exports, __webpack_require__) {

var core  = __webpack_require__(0)
  , $JSON = core.JSON || (core.JSON = {stringify: JSON.stringify});
module.exports = function stringify(it){ // eslint-disable-line no-unused-vars
  return $JSON.stringify.apply($JSON, arguments);
};

/***/ }),
/* 100 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(81);
__webpack_require__(27);
__webpack_require__(37);
__webpack_require__(137);
__webpack_require__(147);
module.exports = __webpack_require__(0).Map;

/***/ }),
/* 101 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(138);
module.exports = __webpack_require__(0).Number.isNaN;

/***/ }),
/* 102 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(139);
module.exports = __webpack_require__(0).Object.assign;

/***/ }),
/* 103 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(140);
var $Object = __webpack_require__(0).Object;
module.exports = function create(P, D){
  return $Object.create(P, D);
};

/***/ }),
/* 104 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(141);
var $Object = __webpack_require__(0).Object;
module.exports = function defineProperty(it, key, desc){
  return $Object.defineProperty(it, key, desc);
};

/***/ }),
/* 105 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(142);
var $Object = __webpack_require__(0).Object;
module.exports = function getOwnPropertyDescriptor(it, key){
  return $Object.getOwnPropertyDescriptor(it, key);
};

/***/ }),
/* 106 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(143);
var $Object = __webpack_require__(0).Object;
module.exports = function getOwnPropertyNames(it){
  return $Object.getOwnPropertyNames(it);
};

/***/ }),
/* 107 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(144);
module.exports = __webpack_require__(0).Object.getPrototypeOf;

/***/ }),
/* 108 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(145);
module.exports = __webpack_require__(0).Object.setPrototypeOf;

/***/ }),
/* 109 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(146);
__webpack_require__(81);
__webpack_require__(148);
__webpack_require__(149);
module.exports = __webpack_require__(0).Symbol;

/***/ }),
/* 110 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(27);
__webpack_require__(37);
module.exports = __webpack_require__(57).f('iterator');

/***/ }),
/* 111 */
/***/ (function(module, exports) {

module.exports = function(it){
  if(typeof it != 'function')throw TypeError(it + ' is not a function!');
  return it;
};

/***/ }),
/* 112 */
/***/ (function(module, exports) {

module.exports = function(){ /* empty */ };

/***/ }),
/* 113 */
/***/ (function(module, exports, __webpack_require__) {

var forOf = __webpack_require__(44);

module.exports = function(iter, ITERATOR){
  var result = [];
  forOf(iter, false, result.push, result, ITERATOR);
  return result;
};


/***/ }),
/* 114 */
/***/ (function(module, exports, __webpack_require__) {

// false -> Array#indexOf
// true  -> Array#includes
var toIObject = __webpack_require__(13)
  , toLength  = __webpack_require__(35)
  , toIndex   = __webpack_require__(132);
module.exports = function(IS_INCLUDES){
  return function($this, el, fromIndex){
    var O      = toIObject($this)
      , length = toLength(O.length)
      , index  = toIndex(fromIndex, length)
      , value;
    // Array#includes uses SameValueZero equality algorithm
    if(IS_INCLUDES && el != el)while(length > index){
      value = O[index++];
      if(value != value)return true;
    // Array#toIndex ignores holes, Array#includes - not
    } else for(;length > index; index++)if(IS_INCLUDES || index in O){
      if(O[index] === el)return IS_INCLUDES || index || 0;
    } return !IS_INCLUDES && -1;
  };
};

/***/ }),
/* 115 */
/***/ (function(module, exports, __webpack_require__) {

// 0 -> Array#forEach
// 1 -> Array#map
// 2 -> Array#filter
// 3 -> Array#some
// 4 -> Array#every
// 5 -> Array#find
// 6 -> Array#findIndex
var ctx      = __webpack_require__(20)
  , IObject  = __webpack_require__(45)
  , toObject = __webpack_require__(26)
  , toLength = __webpack_require__(35)
  , asc      = __webpack_require__(117);
module.exports = function(TYPE, $create){
  var IS_MAP        = TYPE == 1
    , IS_FILTER     = TYPE == 2
    , IS_SOME       = TYPE == 3
    , IS_EVERY      = TYPE == 4
    , IS_FIND_INDEX = TYPE == 6
    , NO_HOLES      = TYPE == 5 || IS_FIND_INDEX
    , create        = $create || asc;
  return function($this, callbackfn, that){
    var O      = toObject($this)
      , self   = IObject(O)
      , f      = ctx(callbackfn, that, 3)
      , length = toLength(self.length)
      , index  = 0
      , result = IS_MAP ? create($this, length) : IS_FILTER ? create($this, 0) : undefined
      , val, res;
    for(;length > index; index++)if(NO_HOLES || index in self){
      val = self[index];
      res = f(val, index, O);
      if(TYPE){
        if(IS_MAP)result[index] = res;            // map
        else if(res)switch(TYPE){
          case 3: return true;                    // some
          case 5: return val;                     // find
          case 6: return index;                   // findIndex
          case 2: result.push(val);               // filter
        } else if(IS_EVERY)return false;          // every
      }
    }
    return IS_FIND_INDEX ? -1 : IS_SOME || IS_EVERY ? IS_EVERY : result;
  };
};

/***/ }),
/* 116 */
/***/ (function(module, exports, __webpack_require__) {

var isObject = __webpack_require__(17)
  , isArray  = __webpack_require__(72)
  , SPECIES  = __webpack_require__(2)('species');

module.exports = function(original){
  var C;
  if(isArray(original)){
    C = original.constructor;
    // cross-realm fallback
    if(typeof C == 'function' && (C === Array || isArray(C.prototype)))C = undefined;
    if(isObject(C)){
      C = C[SPECIES];
      if(C === null)C = undefined;
    }
  } return C === undefined ? Array : C;
};

/***/ }),
/* 117 */
/***/ (function(module, exports, __webpack_require__) {

// 9.4.2.3 ArraySpeciesCreate(originalArray, length)
var speciesConstructor = __webpack_require__(116);

module.exports = function(original, length){
  return new (speciesConstructor(original))(length);
};

/***/ }),
/* 118 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

var dP          = __webpack_require__(5).f
  , create      = __webpack_require__(32)
  , redefineAll = __webpack_require__(79)
  , ctx         = __webpack_require__(20)
  , anInstance  = __webpack_require__(68)
  , defined     = __webpack_require__(31)
  , forOf       = __webpack_require__(44)
  , $iterDefine = __webpack_require__(46)
  , step        = __webpack_require__(74)
  , setSpecies  = __webpack_require__(130)
  , DESCRIPTORS = __webpack_require__(7)
  , fastKey     = __webpack_require__(48).fastKey
  , SIZE        = DESCRIPTORS ? '_s' : 'size';

var getEntry = function(that, key){
  // fast case
  var index = fastKey(key), entry;
  if(index !== 'F')return that._i[index];
  // frozen object case
  for(entry = that._f; entry; entry = entry.n){
    if(entry.k == key)return entry;
  }
};

module.exports = {
  getConstructor: function(wrapper, NAME, IS_MAP, ADDER){
    var C = wrapper(function(that, iterable){
      anInstance(that, C, NAME, '_i');
      that._i = create(null); // index
      that._f = undefined;    // first entry
      that._l = undefined;    // last entry
      that[SIZE] = 0;         // size
      if(iterable != undefined)forOf(iterable, IS_MAP, that[ADDER], that);
    });
    redefineAll(C.prototype, {
      // 23.1.3.1 Map.prototype.clear()
      // 23.2.3.2 Set.prototype.clear()
      clear: function clear(){
        for(var that = this, data = that._i, entry = that._f; entry; entry = entry.n){
          entry.r = true;
          if(entry.p)entry.p = entry.p.n = undefined;
          delete data[entry.i];
        }
        that._f = that._l = undefined;
        that[SIZE] = 0;
      },
      // 23.1.3.3 Map.prototype.delete(key)
      // 23.2.3.4 Set.prototype.delete(value)
      'delete': function(key){
        var that  = this
          , entry = getEntry(that, key);
        if(entry){
          var next = entry.n
            , prev = entry.p;
          delete that._i[entry.i];
          entry.r = true;
          if(prev)prev.n = next;
          if(next)next.p = prev;
          if(that._f == entry)that._f = next;
          if(that._l == entry)that._l = prev;
          that[SIZE]--;
        } return !!entry;
      },
      // 23.2.3.6 Set.prototype.forEach(callbackfn, thisArg = undefined)
      // 23.1.3.5 Map.prototype.forEach(callbackfn, thisArg = undefined)
      forEach: function forEach(callbackfn /*, that = undefined */){
        anInstance(this, C, 'forEach');
        var f = ctx(callbackfn, arguments.length > 1 ? arguments[1] : undefined, 3)
          , entry;
        while(entry = entry ? entry.n : this._f){
          f(entry.v, entry.k, this);
          // revert to the last existing entry
          while(entry && entry.r)entry = entry.p;
        }
      },
      // 23.1.3.7 Map.prototype.has(key)
      // 23.2.3.7 Set.prototype.has(value)
      has: function has(key){
        return !!getEntry(this, key);
      }
    });
    if(DESCRIPTORS)dP(C.prototype, 'size', {
      get: function(){
        return defined(this[SIZE]);
      }
    });
    return C;
  },
  def: function(that, key, value){
    var entry = getEntry(that, key)
      , prev, index;
    // change existing entry
    if(entry){
      entry.v = value;
    // create new entry
    } else {
      that._l = entry = {
        i: index = fastKey(key, true), // <- index
        k: key,                        // <- key
        v: value,                      // <- value
        p: prev = that._l,             // <- previous entry
        n: undefined,                  // <- next entry
        r: false                       // <- removed
      };
      if(!that._f)that._f = entry;
      if(prev)prev.n = entry;
      that[SIZE]++;
      // add to index
      if(index !== 'F')that._i[index] = entry;
    } return that;
  },
  getEntry: getEntry,
  setStrong: function(C, NAME, IS_MAP){
    // add .keys, .values, .entries, [@@iterator]
    // 23.1.3.4, 23.1.3.8, 23.1.3.11, 23.1.3.12, 23.2.3.5, 23.2.3.8, 23.2.3.10, 23.2.3.11
    $iterDefine(C, NAME, function(iterated, kind){
      this._t = iterated;  // target
      this._k = kind;      // kind
      this._l = undefined; // previous
    }, function(){
      var that  = this
        , kind  = that._k
        , entry = that._l;
      // revert to the last existing entry
      while(entry && entry.r)entry = entry.p;
      // get next entry
      if(!that._t || !(that._l = entry = entry ? entry.n : that._t._f)){
        // or finish the iteration
        that._t = undefined;
        return step(1);
      }
      // return step by kind
      if(kind == 'keys'  )return step(0, entry.k);
      if(kind == 'values')return step(0, entry.v);
      return step(0, [entry.k, entry.v]);
    }, IS_MAP ? 'entries' : 'values' , !IS_MAP, true);

    // add [@@species], 23.1.2.2, 23.2.2.2
    setSpecies(NAME);
  }
};

/***/ }),
/* 119 */
/***/ (function(module, exports, __webpack_require__) {

// https://github.com/DavidBruant/Map-Set.prototype.toJSON
var classof = __webpack_require__(41)
  , from    = __webpack_require__(113);
module.exports = function(NAME){
  return function toJSON(){
    if(classof(this) != NAME)throw TypeError(NAME + "#toJSON isn't generic");
    return from(this);
  };
};

/***/ }),
/* 120 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

var global         = __webpack_require__(8)
  , $export        = __webpack_require__(4)
  , meta           = __webpack_require__(48)
  , fails          = __webpack_require__(15)
  , hide           = __webpack_require__(12)
  , redefineAll    = __webpack_require__(79)
  , forOf          = __webpack_require__(44)
  , anInstance     = __webpack_require__(68)
  , isObject       = __webpack_require__(17)
  , setToStringTag = __webpack_require__(34)
  , dP             = __webpack_require__(5).f
  , each           = __webpack_require__(115)(0)
  , DESCRIPTORS    = __webpack_require__(7);

module.exports = function(NAME, wrapper, methods, common, IS_MAP, IS_WEAK){
  var Base  = global[NAME]
    , C     = Base
    , ADDER = IS_MAP ? 'set' : 'add'
    , proto = C && C.prototype
    , O     = {};
  if(!DESCRIPTORS || typeof C != 'function' || !(IS_WEAK || proto.forEach && !fails(function(){
    new C().entries().next();
  }))){
    // create collection constructor
    C = common.getConstructor(wrapper, NAME, IS_MAP, ADDER);
    redefineAll(C.prototype, methods);
    meta.NEED = true;
  } else {
    C = wrapper(function(target, iterable){
      anInstance(target, C, NAME, '_c');
      target._c = new Base;
      if(iterable != undefined)forOf(iterable, IS_MAP, target[ADDER], target);
    });
    each('add,clear,delete,forEach,get,has,set,keys,values,entries,toJSON'.split(','),function(KEY){
      var IS_ADDER = KEY == 'add' || KEY == 'set';
      if(KEY in proto && !(IS_WEAK && KEY == 'clear'))hide(C.prototype, KEY, function(a, b){
        anInstance(this, C, KEY);
        if(!IS_ADDER && IS_WEAK && !isObject(a))return KEY == 'get' ? undefined : false;
        var result = this._c[KEY](a === 0 ? 0 : a, b);
        return IS_ADDER ? this : result;
      });
    });
    if('size' in proto)dP(C.prototype, 'size', {
      get: function(){
        return this._c.size;
      }
    });
  }

  setToStringTag(C, NAME);

  O[NAME] = C;
  $export($export.G + $export.W + $export.F, O);

  if(!IS_WEAK)common.setStrong(C, NAME, IS_MAP);

  return C;
};

/***/ }),
/* 121 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

var $defineProperty = __webpack_require__(5)
  , createDesc      = __webpack_require__(25);

module.exports = function(object, index, value){
  if(index in object)$defineProperty.f(object, index, createDesc(0, value));
  else object[index] = value;
};

/***/ }),
/* 122 */
/***/ (function(module, exports, __webpack_require__) {

// all enumerable object keys, includes symbols
var getKeys = __webpack_require__(24)
  , gOPS    = __webpack_require__(50)
  , pIE     = __webpack_require__(33);
module.exports = function(it){
  var result     = getKeys(it)
    , getSymbols = gOPS.f;
  if(getSymbols){
    var symbols = getSymbols(it)
      , isEnum  = pIE.f
      , i       = 0
      , key;
    while(symbols.length > i)if(isEnum.call(it, key = symbols[i++]))result.push(key);
  } return result;
};

/***/ }),
/* 123 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = __webpack_require__(8).document && document.documentElement;

/***/ }),
/* 124 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

var create         = __webpack_require__(32)
  , descriptor     = __webpack_require__(25)
  , setToStringTag = __webpack_require__(34)
  , IteratorPrototype = {};

// 25.1.2.1.1 %IteratorPrototype%[@@iterator]()
__webpack_require__(12)(IteratorPrototype, __webpack_require__(2)('iterator'), function(){ return this; });

module.exports = function(Constructor, NAME, next){
  Constructor.prototype = create(IteratorPrototype, {next: descriptor(1, next)});
  setToStringTag(Constructor, NAME + ' Iterator');
};

/***/ }),
/* 125 */
/***/ (function(module, exports, __webpack_require__) {

var ITERATOR     = __webpack_require__(2)('iterator')
  , SAFE_CLOSING = false;

try {
  var riter = [7][ITERATOR]();
  riter['return'] = function(){ SAFE_CLOSING = true; };
  Array.from(riter, function(){ throw 2; });
} catch(e){ /* empty */ }

module.exports = function(exec, skipClosing){
  if(!skipClosing && !SAFE_CLOSING)return false;
  var safe = false;
  try {
    var arr  = [7]
      , iter = arr[ITERATOR]();
    iter.next = function(){ return {done: safe = true}; };
    arr[ITERATOR] = function(){ return iter; };
    exec(arr);
  } catch(e){ /* empty */ }
  return safe;
};

/***/ }),
/* 126 */
/***/ (function(module, exports, __webpack_require__) {

var getKeys   = __webpack_require__(24)
  , toIObject = __webpack_require__(13);
module.exports = function(object, el){
  var O      = toIObject(object)
    , keys   = getKeys(O)
    , length = keys.length
    , index  = 0
    , key;
  while(length > index)if(O[key = keys[index++]] === el)return key;
};

/***/ }),
/* 127 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

// 19.1.2.1 Object.assign(target, source, ...)
var getKeys  = __webpack_require__(24)
  , gOPS     = __webpack_require__(50)
  , pIE      = __webpack_require__(33)
  , toObject = __webpack_require__(26)
  , IObject  = __webpack_require__(45)
  , $assign  = Object.assign;

// should work with symbols and should have deterministic property order (V8 bug)
module.exports = !$assign || __webpack_require__(15)(function(){
  var A = {}
    , B = {}
    , S = Symbol()
    , K = 'abcdefghijklmnopqrst';
  A[S] = 7;
  K.split('').forEach(function(k){ B[k] = k; });
  return $assign({}, A)[S] != 7 || Object.keys($assign({}, B)).join('') != K;
}) ? function assign(target, source){ // eslint-disable-line no-unused-vars
  var T     = toObject(target)
    , aLen  = arguments.length
    , index = 1
    , getSymbols = gOPS.f
    , isEnum     = pIE.f;
  while(aLen > index){
    var S      = IObject(arguments[index++])
      , keys   = getSymbols ? getKeys(S).concat(getSymbols(S)) : getKeys(S)
      , length = keys.length
      , j      = 0
      , key;
    while(length > j)if(isEnum.call(S, key = keys[j++]))T[key] = S[key];
  } return T;
} : $assign;

/***/ }),
/* 128 */
/***/ (function(module, exports, __webpack_require__) {

var dP       = __webpack_require__(5)
  , anObject = __webpack_require__(11)
  , getKeys  = __webpack_require__(24);

module.exports = __webpack_require__(7) ? Object.defineProperties : function defineProperties(O, Properties){
  anObject(O);
  var keys   = getKeys(Properties)
    , length = keys.length
    , i = 0
    , P;
  while(length > i)dP.f(O, P = keys[i++], Properties[P]);
  return O;
};

/***/ }),
/* 129 */
/***/ (function(module, exports, __webpack_require__) {

// Works with __proto__ only. Old v8 can't work with null proto objects.
/* eslint-disable no-proto */
var isObject = __webpack_require__(17)
  , anObject = __webpack_require__(11);
var check = function(O, proto){
  anObject(O);
  if(!isObject(proto) && proto !== null)throw TypeError(proto + ": can't set as prototype!");
};
module.exports = {
  set: Object.setPrototypeOf || ('__proto__' in {} ? // eslint-disable-line
    function(test, buggy, set){
      try {
        set = __webpack_require__(20)(Function.call, __webpack_require__(49).f(Object.prototype, '__proto__').set, 2);
        set(test, []);
        buggy = !(test instanceof Array);
      } catch(e){ buggy = true; }
      return function setPrototypeOf(O, proto){
        check(O, proto);
        if(buggy)O.__proto__ = proto;
        else set(O, proto);
        return O;
      };
    }({}, false) : undefined),
  check: check
};

/***/ }),
/* 130 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

var global      = __webpack_require__(8)
  , core        = __webpack_require__(0)
  , dP          = __webpack_require__(5)
  , DESCRIPTORS = __webpack_require__(7)
  , SPECIES     = __webpack_require__(2)('species');

module.exports = function(KEY){
  var C = typeof core[KEY] == 'function' ? core[KEY] : global[KEY];
  if(DESCRIPTORS && C && !C[SPECIES])dP.f(C, SPECIES, {
    configurable: true,
    get: function(){ return this; }
  });
};

/***/ }),
/* 131 */
/***/ (function(module, exports, __webpack_require__) {

var toInteger = __webpack_require__(54)
  , defined   = __webpack_require__(31);
// true  -> String#at
// false -> String#codePointAt
module.exports = function(TO_STRING){
  return function(that, pos){
    var s = String(defined(that))
      , i = toInteger(pos)
      , l = s.length
      , a, b;
    if(i < 0 || i >= l)return TO_STRING ? '' : undefined;
    a = s.charCodeAt(i);
    return a < 0xd800 || a > 0xdbff || i + 1 === l || (b = s.charCodeAt(i + 1)) < 0xdc00 || b > 0xdfff
      ? TO_STRING ? s.charAt(i) : a
      : TO_STRING ? s.slice(i, i + 2) : (a - 0xd800 << 10) + (b - 0xdc00) + 0x10000;
  };
};

/***/ }),
/* 132 */
/***/ (function(module, exports, __webpack_require__) {

var toInteger = __webpack_require__(54)
  , max       = Math.max
  , min       = Math.min;
module.exports = function(index, length){
  index = toInteger(index);
  return index < 0 ? max(index + length, 0) : min(index, length);
};

/***/ }),
/* 133 */
/***/ (function(module, exports, __webpack_require__) {

var anObject = __webpack_require__(11)
  , get      = __webpack_require__(58);
module.exports = __webpack_require__(0).getIterator = function(it){
  var iterFn = get(it);
  if(typeof iterFn != 'function')throw TypeError(it + ' is not iterable!');
  return anObject(iterFn.call(it));
};

/***/ }),
/* 134 */
/***/ (function(module, exports, __webpack_require__) {

var classof   = __webpack_require__(41)
  , ITERATOR  = __webpack_require__(2)('iterator')
  , Iterators = __webpack_require__(21);
module.exports = __webpack_require__(0).isIterable = function(it){
  var O = Object(it);
  return O[ITERATOR] !== undefined
    || '@@iterator' in O
    || Iterators.hasOwnProperty(classof(O));
};

/***/ }),
/* 135 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

var ctx            = __webpack_require__(20)
  , $export        = __webpack_require__(4)
  , toObject       = __webpack_require__(26)
  , call           = __webpack_require__(73)
  , isArrayIter    = __webpack_require__(71)
  , toLength       = __webpack_require__(35)
  , createProperty = __webpack_require__(121)
  , getIterFn      = __webpack_require__(58);

$export($export.S + $export.F * !__webpack_require__(125)(function(iter){ Array.from(iter); }), 'Array', {
  // 22.1.2.1 Array.from(arrayLike, mapfn = undefined, thisArg = undefined)
  from: function from(arrayLike/*, mapfn = undefined, thisArg = undefined*/){
    var O       = toObject(arrayLike)
      , C       = typeof this == 'function' ? this : Array
      , aLen    = arguments.length
      , mapfn   = aLen > 1 ? arguments[1] : undefined
      , mapping = mapfn !== undefined
      , index   = 0
      , iterFn  = getIterFn(O)
      , length, result, step, iterator;
    if(mapping)mapfn = ctx(mapfn, aLen > 2 ? arguments[2] : undefined, 2);
    // if object isn't iterable or it's array with default iterator - use simple case
    if(iterFn != undefined && !(C == Array && isArrayIter(iterFn))){
      for(iterator = iterFn.call(O), result = new C; !(step = iterator.next()).done; index++){
        createProperty(result, index, mapping ? call(iterator, mapfn, [step.value, index], true) : step.value);
      }
    } else {
      length = toLength(O.length);
      for(result = new C(length); length > index; index++){
        createProperty(result, index, mapping ? mapfn(O[index], index) : O[index]);
      }
    }
    result.length = index;
    return result;
  }
});


/***/ }),
/* 136 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

var addToUnscopables = __webpack_require__(112)
  , step             = __webpack_require__(74)
  , Iterators        = __webpack_require__(21)
  , toIObject        = __webpack_require__(13);

// 22.1.3.4 Array.prototype.entries()
// 22.1.3.13 Array.prototype.keys()
// 22.1.3.29 Array.prototype.values()
// 22.1.3.30 Array.prototype[@@iterator]()
module.exports = __webpack_require__(46)(Array, 'Array', function(iterated, kind){
  this._t = toIObject(iterated); // target
  this._i = 0;                   // next index
  this._k = kind;                // kind
// 22.1.5.2.1 %ArrayIteratorPrototype%.next()
}, function(){
  var O     = this._t
    , kind  = this._k
    , index = this._i++;
  if(!O || index >= O.length){
    this._t = undefined;
    return step(1);
  }
  if(kind == 'keys'  )return step(0, index);
  if(kind == 'values')return step(0, O[index]);
  return step(0, [index, O[index]]);
}, 'values');

// argumentsList[@@iterator] is %ArrayProto_values% (9.4.4.6, 9.4.4.7)
Iterators.Arguments = Iterators.Array;

addToUnscopables('keys');
addToUnscopables('values');
addToUnscopables('entries');

/***/ }),
/* 137 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

var strong = __webpack_require__(118);

// 23.1 Map Objects
module.exports = __webpack_require__(120)('Map', function(get){
  return function Map(){ return get(this, arguments.length > 0 ? arguments[0] : undefined); };
}, {
  // 23.1.3.6 Map.prototype.get(key)
  get: function get(key){
    var entry = strong.getEntry(this, key);
    return entry && entry.v;
  },
  // 23.1.3.9 Map.prototype.set(key, value)
  set: function set(key, value){
    return strong.def(this, key === 0 ? 0 : key, value);
  }
}, strong, true);

/***/ }),
/* 138 */
/***/ (function(module, exports, __webpack_require__) {

// 20.1.2.4 Number.isNaN(number)
var $export = __webpack_require__(4);

$export($export.S, 'Number', {
  isNaN: function isNaN(number){
    return number != number;
  }
});

/***/ }),
/* 139 */
/***/ (function(module, exports, __webpack_require__) {

// 19.1.3.1 Object.assign(target, source)
var $export = __webpack_require__(4);

$export($export.S + $export.F, 'Object', {assign: __webpack_require__(127)});

/***/ }),
/* 140 */
/***/ (function(module, exports, __webpack_require__) {

var $export = __webpack_require__(4)
// 19.1.2.2 / 15.2.3.5 Object.create(O [, Properties])
$export($export.S, 'Object', {create: __webpack_require__(32)});

/***/ }),
/* 141 */
/***/ (function(module, exports, __webpack_require__) {

var $export = __webpack_require__(4);
// 19.1.2.4 / 15.2.3.6 Object.defineProperty(O, P, Attributes)
$export($export.S + $export.F * !__webpack_require__(7), 'Object', {defineProperty: __webpack_require__(5).f});

/***/ }),
/* 142 */
/***/ (function(module, exports, __webpack_require__) {

// 19.1.2.6 Object.getOwnPropertyDescriptor(O, P)
var toIObject                 = __webpack_require__(13)
  , $getOwnPropertyDescriptor = __webpack_require__(49).f;

__webpack_require__(51)('getOwnPropertyDescriptor', function(){
  return function getOwnPropertyDescriptor(it, key){
    return $getOwnPropertyDescriptor(toIObject(it), key);
  };
});

/***/ }),
/* 143 */
/***/ (function(module, exports, __webpack_require__) {

// 19.1.2.7 Object.getOwnPropertyNames(O)
__webpack_require__(51)('getOwnPropertyNames', function(){
  return __webpack_require__(75).f;
});

/***/ }),
/* 144 */
/***/ (function(module, exports, __webpack_require__) {

// 19.1.2.9 Object.getPrototypeOf(O)
var toObject        = __webpack_require__(26)
  , $getPrototypeOf = __webpack_require__(77);

__webpack_require__(51)('getPrototypeOf', function(){
  return function getPrototypeOf(it){
    return $getPrototypeOf(toObject(it));
  };
});

/***/ }),
/* 145 */
/***/ (function(module, exports, __webpack_require__) {

// 19.1.3.19 Object.setPrototypeOf(O, proto)
var $export = __webpack_require__(4);
$export($export.S, 'Object', {setPrototypeOf: __webpack_require__(129).set});

/***/ }),
/* 146 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

// ECMAScript 6 symbols shim
var global         = __webpack_require__(8)
  , has            = __webpack_require__(16)
  , DESCRIPTORS    = __webpack_require__(7)
  , $export        = __webpack_require__(4)
  , redefine       = __webpack_require__(80)
  , META           = __webpack_require__(48).KEY
  , $fails         = __webpack_require__(15)
  , shared         = __webpack_require__(53)
  , setToStringTag = __webpack_require__(34)
  , uid            = __webpack_require__(36)
  , wks            = __webpack_require__(2)
  , wksExt         = __webpack_require__(57)
  , wksDefine      = __webpack_require__(56)
  , keyOf          = __webpack_require__(126)
  , enumKeys       = __webpack_require__(122)
  , isArray        = __webpack_require__(72)
  , anObject       = __webpack_require__(11)
  , toIObject      = __webpack_require__(13)
  , toPrimitive    = __webpack_require__(55)
  , createDesc     = __webpack_require__(25)
  , _create        = __webpack_require__(32)
  , gOPNExt        = __webpack_require__(75)
  , $GOPD          = __webpack_require__(49)
  , $DP            = __webpack_require__(5)
  , $keys          = __webpack_require__(24)
  , gOPD           = $GOPD.f
  , dP             = $DP.f
  , gOPN           = gOPNExt.f
  , $Symbol        = global.Symbol
  , $JSON          = global.JSON
  , _stringify     = $JSON && $JSON.stringify
  , PROTOTYPE      = 'prototype'
  , HIDDEN         = wks('_hidden')
  , TO_PRIMITIVE   = wks('toPrimitive')
  , isEnum         = {}.propertyIsEnumerable
  , SymbolRegistry = shared('symbol-registry')
  , AllSymbols     = shared('symbols')
  , OPSymbols      = shared('op-symbols')
  , ObjectProto    = Object[PROTOTYPE]
  , USE_NATIVE     = typeof $Symbol == 'function'
  , QObject        = global.QObject;
// Don't use setters in Qt Script, https://github.com/zloirock/core-js/issues/173
var setter = !QObject || !QObject[PROTOTYPE] || !QObject[PROTOTYPE].findChild;

// fallback for old Android, https://code.google.com/p/v8/issues/detail?id=687
var setSymbolDesc = DESCRIPTORS && $fails(function(){
  return _create(dP({}, 'a', {
    get: function(){ return dP(this, 'a', {value: 7}).a; }
  })).a != 7;
}) ? function(it, key, D){
  var protoDesc = gOPD(ObjectProto, key);
  if(protoDesc)delete ObjectProto[key];
  dP(it, key, D);
  if(protoDesc && it !== ObjectProto)dP(ObjectProto, key, protoDesc);
} : dP;

var wrap = function(tag){
  var sym = AllSymbols[tag] = _create($Symbol[PROTOTYPE]);
  sym._k = tag;
  return sym;
};

var isSymbol = USE_NATIVE && typeof $Symbol.iterator == 'symbol' ? function(it){
  return typeof it == 'symbol';
} : function(it){
  return it instanceof $Symbol;
};

var $defineProperty = function defineProperty(it, key, D){
  if(it === ObjectProto)$defineProperty(OPSymbols, key, D);
  anObject(it);
  key = toPrimitive(key, true);
  anObject(D);
  if(has(AllSymbols, key)){
    if(!D.enumerable){
      if(!has(it, HIDDEN))dP(it, HIDDEN, createDesc(1, {}));
      it[HIDDEN][key] = true;
    } else {
      if(has(it, HIDDEN) && it[HIDDEN][key])it[HIDDEN][key] = false;
      D = _create(D, {enumerable: createDesc(0, false)});
    } return setSymbolDesc(it, key, D);
  } return dP(it, key, D);
};
var $defineProperties = function defineProperties(it, P){
  anObject(it);
  var keys = enumKeys(P = toIObject(P))
    , i    = 0
    , l = keys.length
    , key;
  while(l > i)$defineProperty(it, key = keys[i++], P[key]);
  return it;
};
var $create = function create(it, P){
  return P === undefined ? _create(it) : $defineProperties(_create(it), P);
};
var $propertyIsEnumerable = function propertyIsEnumerable(key){
  var E = isEnum.call(this, key = toPrimitive(key, true));
  if(this === ObjectProto && has(AllSymbols, key) && !has(OPSymbols, key))return false;
  return E || !has(this, key) || !has(AllSymbols, key) || has(this, HIDDEN) && this[HIDDEN][key] ? E : true;
};
var $getOwnPropertyDescriptor = function getOwnPropertyDescriptor(it, key){
  it  = toIObject(it);
  key = toPrimitive(key, true);
  if(it === ObjectProto && has(AllSymbols, key) && !has(OPSymbols, key))return;
  var D = gOPD(it, key);
  if(D && has(AllSymbols, key) && !(has(it, HIDDEN) && it[HIDDEN][key]))D.enumerable = true;
  return D;
};
var $getOwnPropertyNames = function getOwnPropertyNames(it){
  var names  = gOPN(toIObject(it))
    , result = []
    , i      = 0
    , key;
  while(names.length > i){
    if(!has(AllSymbols, key = names[i++]) && key != HIDDEN && key != META)result.push(key);
  } return result;
};
var $getOwnPropertySymbols = function getOwnPropertySymbols(it){
  var IS_OP  = it === ObjectProto
    , names  = gOPN(IS_OP ? OPSymbols : toIObject(it))
    , result = []
    , i      = 0
    , key;
  while(names.length > i){
    if(has(AllSymbols, key = names[i++]) && (IS_OP ? has(ObjectProto, key) : true))result.push(AllSymbols[key]);
  } return result;
};

// 19.4.1.1 Symbol([description])
if(!USE_NATIVE){
  $Symbol = function Symbol(){
    if(this instanceof $Symbol)throw TypeError('Symbol is not a constructor!');
    var tag = uid(arguments.length > 0 ? arguments[0] : undefined);
    var $set = function(value){
      if(this === ObjectProto)$set.call(OPSymbols, value);
      if(has(this, HIDDEN) && has(this[HIDDEN], tag))this[HIDDEN][tag] = false;
      setSymbolDesc(this, tag, createDesc(1, value));
    };
    if(DESCRIPTORS && setter)setSymbolDesc(ObjectProto, tag, {configurable: true, set: $set});
    return wrap(tag);
  };
  redefine($Symbol[PROTOTYPE], 'toString', function toString(){
    return this._k;
  });

  $GOPD.f = $getOwnPropertyDescriptor;
  $DP.f   = $defineProperty;
  __webpack_require__(76).f = gOPNExt.f = $getOwnPropertyNames;
  __webpack_require__(33).f  = $propertyIsEnumerable;
  __webpack_require__(50).f = $getOwnPropertySymbols;

  if(DESCRIPTORS && !__webpack_require__(47)){
    redefine(ObjectProto, 'propertyIsEnumerable', $propertyIsEnumerable, true);
  }

  wksExt.f = function(name){
    return wrap(wks(name));
  }
}

$export($export.G + $export.W + $export.F * !USE_NATIVE, {Symbol: $Symbol});

for(var symbols = (
  // 19.4.2.2, 19.4.2.3, 19.4.2.4, 19.4.2.6, 19.4.2.8, 19.4.2.9, 19.4.2.10, 19.4.2.11, 19.4.2.12, 19.4.2.13, 19.4.2.14
  'hasInstance,isConcatSpreadable,iterator,match,replace,search,species,split,toPrimitive,toStringTag,unscopables'
).split(','), i = 0; symbols.length > i; )wks(symbols[i++]);

for(var symbols = $keys(wks.store), i = 0; symbols.length > i; )wksDefine(symbols[i++]);

$export($export.S + $export.F * !USE_NATIVE, 'Symbol', {
  // 19.4.2.1 Symbol.for(key)
  'for': function(key){
    return has(SymbolRegistry, key += '')
      ? SymbolRegistry[key]
      : SymbolRegistry[key] = $Symbol(key);
  },
  // 19.4.2.5 Symbol.keyFor(sym)
  keyFor: function keyFor(key){
    if(isSymbol(key))return keyOf(SymbolRegistry, key);
    throw TypeError(key + ' is not a symbol!');
  },
  useSetter: function(){ setter = true; },
  useSimple: function(){ setter = false; }
});

$export($export.S + $export.F * !USE_NATIVE, 'Object', {
  // 19.1.2.2 Object.create(O [, Properties])
  create: $create,
  // 19.1.2.4 Object.defineProperty(O, P, Attributes)
  defineProperty: $defineProperty,
  // 19.1.2.3 Object.defineProperties(O, Properties)
  defineProperties: $defineProperties,
  // 19.1.2.6 Object.getOwnPropertyDescriptor(O, P)
  getOwnPropertyDescriptor: $getOwnPropertyDescriptor,
  // 19.1.2.7 Object.getOwnPropertyNames(O)
  getOwnPropertyNames: $getOwnPropertyNames,
  // 19.1.2.8 Object.getOwnPropertySymbols(O)
  getOwnPropertySymbols: $getOwnPropertySymbols
});

// 24.3.2 JSON.stringify(value [, replacer [, space]])
$JSON && $export($export.S + $export.F * (!USE_NATIVE || $fails(function(){
  var S = $Symbol();
  // MS Edge converts symbol values to JSON as {}
  // WebKit converts symbol values to JSON as null
  // V8 throws on boxed symbols
  return _stringify([S]) != '[null]' || _stringify({a: S}) != '{}' || _stringify(Object(S)) != '{}';
})), 'JSON', {
  stringify: function stringify(it){
    if(it === undefined || isSymbol(it))return; // IE8 returns string on undefined
    var args = [it]
      , i    = 1
      , replacer, $replacer;
    while(arguments.length > i)args.push(arguments[i++]);
    replacer = args[1];
    if(typeof replacer == 'function')$replacer = replacer;
    if($replacer || !isArray(replacer))replacer = function(key, value){
      if($replacer)value = $replacer.call(this, key, value);
      if(!isSymbol(value))return value;
    };
    args[1] = replacer;
    return _stringify.apply($JSON, args);
  }
});

// 19.4.3.4 Symbol.prototype[@@toPrimitive](hint)
$Symbol[PROTOTYPE][TO_PRIMITIVE] || __webpack_require__(12)($Symbol[PROTOTYPE], TO_PRIMITIVE, $Symbol[PROTOTYPE].valueOf);
// 19.4.3.5 Symbol.prototype[@@toStringTag]
setToStringTag($Symbol, 'Symbol');
// 20.2.1.9 Math[@@toStringTag]
setToStringTag(Math, 'Math', true);
// 24.3.3 JSON[@@toStringTag]
setToStringTag(global.JSON, 'JSON', true);

/***/ }),
/* 147 */
/***/ (function(module, exports, __webpack_require__) {

// https://github.com/DavidBruant/Map-Set.prototype.toJSON
var $export  = __webpack_require__(4);

$export($export.P + $export.R, 'Map', {toJSON: __webpack_require__(119)('Map')});

/***/ }),
/* 148 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(56)('asyncIterator');

/***/ }),
/* 149 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(56)('observable');

/***/ }),
/* 150 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
Object.defineProperty(__webpack_exports__, "__esModule", { value: true });
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0__src_App_fs__ = __webpack_require__(82);
/* harmony namespace reexport (by provided) */ __webpack_require__.d(__webpack_exports__, "init", function() { return __WEBPACK_IMPORTED_MODULE_0__src_App_fs__["a"]; });


/***/ }),
/* 151 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* unused harmony export StringEx */
/* unused harmony export ParseStatus */
/* unused harmony export ParserResult */
/* unused harmony export Result */
/* harmony export (immutable) */ __webpack_exports__["i"] = op_GreaterTwiddleGreater;
/* unused harmony export parse */
/* harmony export (immutable) */ __webpack_exports__["n"] = op_LessBangBangGreater;
/* unused harmony export getNames */
/* harmony export (immutable) */ __webpack_exports__["l"] = pChar;
/* unused harmony export pString */
/* unused harmony export any */
/* unused harmony export anyChar */
/* unused harmony export anyNumber */
/* unused harmony export all */
/* harmony export (immutable) */ __webpack_exports__["g"] = choice;
/* harmony export (immutable) */ __webpack_exports__["k"] = op_GreaterMinusGreater;
/* harmony export (immutable) */ __webpack_exports__["j"] = op_LessMinusLess;
/* unused harmony export op_LessMinusGreater */
/* unused harmony export op_BangBang */
/* harmony export (immutable) */ __webpack_exports__["b"] = op_EqualsEqualsGreater;
/* unused harmony export between */
/* harmony export (immutable) */ __webpack_exports__["f"] = betweenChar;
/* harmony export (immutable) */ __webpack_exports__["c"] = betweenStr;
/* harmony export (immutable) */ __webpack_exports__["d"] = sepBy;
/* unused harmony export sepByChar */
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "m", function() { return anyStr; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "e", function() { return spaces1; });
/* unused harmony export spaces */
/* harmony export (immutable) */ __webpack_exports__["a"] = refl;
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "h", function() { return pfloat; });
/* unused harmony export pint */
/* unused harmony export pbool */
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_array_from__ = __webpack_require__(19);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_array_from___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_array_from__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_object_set_prototype_of__ = __webpack_require__(65);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_object_set_prototype_of___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_object_set_prototype_of__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_babel_runtime_core_js_object_get_prototype_of__ = __webpack_require__(64);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_babel_runtime_core_js_object_get_prototype_of___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_2_babel_runtime_core_js_object_get_prototype_of__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3_babel_runtime_helpers_classCallCheck__ = __webpack_require__(9);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3_babel_runtime_helpers_classCallCheck___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_3_babel_runtime_helpers_classCallCheck__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4_babel_runtime_helpers_createClass__ = __webpack_require__(10);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4_babel_runtime_helpers_createClass___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_4_babel_runtime_helpers_createClass__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_5_babel_runtime_helpers_possibleConstructorReturn__ = __webpack_require__(94);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_5_babel_runtime_helpers_possibleConstructorReturn___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_5_babel_runtime_helpers_possibleConstructorReturn__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_6_babel_runtime_helpers_inherits__ = __webpack_require__(93);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_6_babel_runtime_helpers_inherits___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_6_babel_runtime_helpers_inherits__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_7__packages_Fable_Core_fable_core_Symbol__ = __webpack_require__(6);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_8__packages_Fable_Core_fable_core_Util__ = __webpack_require__(1);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__ = __webpack_require__(40);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_10__packages_Fable_Core_fable_core_Map__ = __webpack_require__(29);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_11__packages_Fable_Core_fable_core_Comparer__ = __webpack_require__(38);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_12__packages_Fable_Core_fable_core_List__ = __webpack_require__(28);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_13__packages_Fable_Core_fable_core_Seq__ = __webpack_require__(3);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_14__packages_Fable_Core_fable_core_Double__ = __webpack_require__(83);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_15__packages_Fable_Core_fable_core_Long__ = __webpack_require__(39);



















var StringEx = function (__exports) {
    var OutOfRangeException = __exports.OutOfRangeException = function (_Error) {
        __WEBPACK_IMPORTED_MODULE_6_babel_runtime_helpers_inherits___default()(OutOfRangeException, _Error);

        function OutOfRangeException(data0, index) {
            __WEBPACK_IMPORTED_MODULE_3_babel_runtime_helpers_classCallCheck___default()(this, OutOfRangeException);

            var _this = __WEBPACK_IMPORTED_MODULE_5_babel_runtime_helpers_possibleConstructorReturn___default()(this, (OutOfRangeException.__proto__ || __WEBPACK_IMPORTED_MODULE_2_babel_runtime_core_js_object_get_prototype_of___default()(OutOfRangeException)).call(this));

            __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_object_set_prototype_of___default()(_this, OutOfRangeException.prototype);

            _this.Data0 = data0;
            _this.index = index | 0;
            return _this;
        }

        __WEBPACK_IMPORTED_MODULE_4_babel_runtime_helpers_createClass___default()(OutOfRangeException, [{
            key: __WEBPACK_IMPORTED_MODULE_7__packages_Fable_Core_fable_core_Symbol__["a" /* default */].reflection,
            value: function value() {
                return {
                    type: "mparsec.StringEx.OutOfRangeException",
                    interfaces: ["FSharpException"],
                    properties: {
                        Data0: "string",
                        index: "number"
                    }
                };
            }
        }]);

        return OutOfRangeException;
    }(Error);

    __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__packages_Fable_Core_fable_core_Symbol__["b" /* setType */])("mparsec.StringEx.OutOfRangeException", OutOfRangeException);

    var StringEx = function () {
        function StringEx(str, index) {
            __WEBPACK_IMPORTED_MODULE_3_babel_runtime_helpers_classCallCheck___default()(this, StringEx);

            this.str = str;
            this.index = index | 0;
        }

        __WEBPACK_IMPORTED_MODULE_4_babel_runtime_helpers_createClass___default()(StringEx, [{
            key: __WEBPACK_IMPORTED_MODULE_7__packages_Fable_Core_fable_core_Symbol__["a" /* default */].reflection,
            value: function value() {
                return {
                    type: "mparsec.StringEx.StringEx",
                    interfaces: ["FSharpRecord", "System.IEquatable", "System.IComparable"],
                    properties: {
                        str: "string",
                        index: "number"
                    }
                };
            }
        }, {
            key: "Equals",
            value: function Equals(other) {
                return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__packages_Fable_Core_fable_core_Util__["i" /* equalsRecords */])(this, other);
            }
        }, {
            key: "CompareTo",
            value: function CompareTo(other) {
                return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__packages_Fable_Core_fable_core_Util__["j" /* compareRecords */])(this, other) | 0;
            }
        }, {
            key: "ToString",
            value: function ToString() {
                return this.str.substr(this.index);
            }
        }, {
            key: "get_Item",
            value: function get_Item(index) {
                return this.str[this.index + index];
            }
        }, {
            key: "Substring",
            value: function Substring(index) {
                if (index <= this.Length) {
                    var index_1 = this.index + index | 0;
                    return new StringEx(this.str, index_1);
                } else {
                    throw new OutOfRangeException(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__packages_Fable_Core_fable_core_Util__["a" /* toString */])(this), index);
                }
            }
        }, {
            key: "AsString",
            get: function get() {
                return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__packages_Fable_Core_fable_core_Util__["a" /* toString */])(this);
            }
        }, {
            key: "Length",
            get: function get() {
                return this.str.length - this.index | 0;
            }
        }, {
            key: "Position",
            get: function get() {
                return this.index | 0;
            }
        }]);

        return StringEx;
    }();

    __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__packages_Fable_Core_fable_core_Symbol__["b" /* setType */])("mparsec.StringEx.StringEx", StringEx);

    var toStringEx = __exports.toStringEx = function (str) {
        return new StringEx(str, 0);
    };

    var substring = __exports.substring = function (s) {
        return function (arg00) {
            return s.Substring(arg00);
        };
    };

    var startsWith = __exports.startsWith = function (txt, s) {
        return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["d" /* startsWith */])(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__packages_Fable_Core_fable_core_Util__["a" /* toString */])(s), txt, 5);
    };

    return __exports;
}({});
var ParseStatus = function () {
    function ParseStatus(trampoline, text) {
        __WEBPACK_IMPORTED_MODULE_3_babel_runtime_helpers_classCallCheck___default()(this, ParseStatus);

        this.Trampoline = trampoline;
        this.Text = text;
    }

    __WEBPACK_IMPORTED_MODULE_4_babel_runtime_helpers_createClass___default()(ParseStatus, [{
        key: __WEBPACK_IMPORTED_MODULE_7__packages_Fable_Core_fable_core_Symbol__["a" /* default */].reflection,
        value: function value() {
            return {
                type: "mparsec.ParseStatus",
                interfaces: ["FSharpRecord", "System.IEquatable", "System.IComparable"],
                properties: {
                    Trampoline: __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__packages_Fable_Core_fable_core_Util__["e" /* makeGeneric */])(__WEBPACK_IMPORTED_MODULE_10__packages_Fable_Core_fable_core_Map__["d" /* default */], {
                        Key: __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__packages_Fable_Core_fable_core_Util__["k" /* Tuple */])(["string", "number"]),
                        Value: "number"
                    }),
                    Text: StringEx.StringEx
                }
            };
        }
    }, {
        key: "Equals",
        value: function Equals(other) {
            return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__packages_Fable_Core_fable_core_Util__["i" /* equalsRecords */])(this, other);
        }
    }, {
        key: "CompareTo",
        value: function CompareTo(other) {
            return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__packages_Fable_Core_fable_core_Util__["j" /* compareRecords */])(this, other) | 0;
        }
    }]);

    return ParseStatus;
}();
__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__packages_Fable_Core_fable_core_Symbol__["b" /* setType */])("mparsec.ParseStatus", ParseStatus);
var ParserResult = function () {
    function ParserResult(tag, data) {
        __WEBPACK_IMPORTED_MODULE_3_babel_runtime_helpers_classCallCheck___default()(this, ParserResult);

        this.tag = tag;
        this.data = data;
    }

    __WEBPACK_IMPORTED_MODULE_4_babel_runtime_helpers_createClass___default()(ParserResult, [{
        key: __WEBPACK_IMPORTED_MODULE_7__packages_Fable_Core_fable_core_Symbol__["a" /* default */].reflection,
        value: function value() {
            return {
                type: "mparsec.ParserResult",
                interfaces: ["FSharpUnion", "System.IEquatable", "System.IComparable"],
                cases: [["Parsed", __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__packages_Fable_Core_fable_core_Util__["k" /* Tuple */])([__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__packages_Fable_Core_fable_core_Util__["l" /* GenericParam */])("u"), ParseStatus])], ["Failed", "string", StringEx.StringEx]]
            };
        }
    }, {
        key: "Equals",
        value: function Equals(other) {
            return this === other || this.tag === other.tag && __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__packages_Fable_Core_fable_core_Util__["c" /* equals */])(this.data, other.data);
        }
    }, {
        key: "CompareTo",
        value: function CompareTo(other) {
            return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__packages_Fable_Core_fable_core_Util__["d" /* compareUnions */])(this, other) | 0;
        }
    }, {
        key: "AsString",
        get: function get() {
            if (this.tag === 1) {
                return {
                    formatFn: __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["a" /* fsFormat */])("Failed (reason = %s, position = %d, remaining = %O)"),
                    input: "Failed (reason = %s, position = %d, remaining = %O)"
                }.formatFn(function (x) {
                    return x;
                })(this.data[0], this.data[1].Position, this.data[1]);
            } else {
                var v = this.data[0];
                var str = this.data[1];
                return {
                    formatFn: __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["a" /* fsFormat */])("OK (%A, remaining = %O)"),
                    input: "OK (%A, remaining = %O)"
                }.formatFn(function (x) {
                    return x;
                })(v, str);
            }
        }
    }]);

    return ParserResult;
}();
__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__packages_Fable_Core_fable_core_Symbol__["b" /* setType */])("mparsec.ParserResult", ParserResult);
var Result = function (__exports) {
    var isParsed = __exports.isParsed = function (r) {
        if (r.tag === 0) {
            return true;
        } else {
            return false;
        }
    };

    var isFailed = __exports.isFailed = function (r) {
        if (r.tag === 1) {
            return true;
        } else {
            return false;
        }
    };

    var bind = __exports.bind = function (binder, bind_1) {
        if (bind_1.tag === 1) {
            return new ParserResult(1, [bind_1.data[0], bind_1.data[1]]);
        } else {
            var v = bind_1.data[0];
            var txt = bind_1.data[1];
            return binder([v, txt]);
        }
    };

    var map = __exports.map = function (mapper, v) {
        if (v.tag === 1) {
            return new ParserResult(1, [v.data[0], v.data[1]]);
        } else {
            var v_1 = v.data[0];
            var txt = v.data[1];
            return new ParserResult(0, [mapper(v_1), txt]);
        }
    };

    var defaultValue = __exports.defaultValue = function (def_0, def_1, result) {
        var def = [def_0, def_1];

        if (result.tag === 1) {
            return def;
        } else {
            var v = result.data[0];
            var txt = result.data[1];
            return [v, txt];
        }
    };

    return __exports;
}({});

var Parser = function () {
    function Parser(parser, name, id) {
        __WEBPACK_IMPORTED_MODULE_3_babel_runtime_helpers_classCallCheck___default()(this, Parser);

        this.parser = parser;
        this.name = name;
        this.id = id;
    }

    __WEBPACK_IMPORTED_MODULE_4_babel_runtime_helpers_createClass___default()(Parser, [{
        key: __WEBPACK_IMPORTED_MODULE_7__packages_Fable_Core_fable_core_Symbol__["a" /* default */].reflection,
        value: function value() {
            return {
                type: "mparsec.Parser",
                interfaces: ["FSharpRecord"],
                properties: {
                    parser: __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__packages_Fable_Core_fable_core_Util__["m" /* Function */])([ParseStatus, __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__packages_Fable_Core_fable_core_Util__["e" /* makeGeneric */])(ParserResult, {
                        u: __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__packages_Fable_Core_fable_core_Util__["l" /* GenericParam */])("u")
                    })]),
                    name: "string",
                    id: "string"
                }
            };
        }
    }, {
        key: "f",
        value: function f(status) {
            var _this2 = this;

            var call = function call(c) {
                var status_1 = new ParseStatus(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_10__packages_Fable_Core_fable_core_Map__["b" /* add */])([_this2.id, status.Text.Position], c + 1, status.Trampoline), status.Text);
                return _this2.parser(status_1);
            };

            var matchValue = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_10__packages_Fable_Core_fable_core_Map__["a" /* tryFind */])([this.id, status.Text.Position], status.Trampoline);
            var $var1 = matchValue != null ? matchValue > 3 ? [0, matchValue] : [1] : [1];

            switch ($var1[0]) {
                case 0:
                    return new ParserResult(1, ["dup", status.Text]);

                case 1:
                    if (matchValue == null) {
                        return call(0);
                    } else {
                        return call(matchValue);
                    }

            }
        }
    }]);

    return Parser;
}();

__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__packages_Fable_Core_fable_core_Symbol__["b" /* setType */])("mparsec.Parser", Parser);
function op_GreaterTwiddleGreater(p, name) {
    return new Parser(p.parser, name, p.id);
}
function parse(p, txt) {
    return p.parser(function () {
        var _Text = StringEx.toStringEx(txt);

        return new ParseStatus(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_10__packages_Fable_Core_fable_core_Map__["c" /* create */])(null, new __WEBPACK_IMPORTED_MODULE_11__packages_Fable_Core_fable_core_Comparer__["a" /* default */](__WEBPACK_IMPORTED_MODULE_8__packages_Fable_Core_fable_core_Util__["b" /* compare */])), _Text);
    }());
}
function op_LessBangBangGreater(p, txt) {
    return parse(p, txt);
}
function getNames(parsers) {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["e" /* join */])(", ", __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_12__packages_Fable_Core_fable_core_List__["c" /* map */])(function (x) {
        return x.name;
    }, parsers));
}
function pChar(ch) {
    var name = {
        formatFn: __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["a" /* fsFormat */])("pChar %c"),
        input: "pChar %c"
    }.formatFn(function (x) {
        return x;
    })(ch);
    var id = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["f" /* newGuid */])();
    return new Parser(function (status) {
        return (status.Text.Length > 0 ? status.Text.get_Item(0) === ch : false) ? new ParserResult(0, [ch, function () {
            var _Text = StringEx.substring(status.Text)(1);

            return new ParseStatus(status.Trampoline, _Text);
        }()]) : new ParserResult(1, [{
            formatFn: __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["a" /* fsFormat */])("Cannot match '%c'"),
            input: "Cannot match '%c'"
        }.formatFn(function (x) {
            return x;
        })(ch), status.Text]);
    }, name, id);
}
function pString(str) {
    var name = {
        formatFn: __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["a" /* fsFormat */])("pString %s"),
        input: "pString %s"
    }.formatFn(function (x) {
        return x;
    })(str);
    var id = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["f" /* newGuid */])();
    return new Parser(function (status) {
        return StringEx.startsWith(str, status.Text) ? new ParserResult(0, [str, function () {
            var _Text = StringEx.substring(status.Text)(str.length);

            return new ParseStatus(status.Trampoline, _Text);
        }()]) : new ParserResult(1, [{
            formatFn: __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["a" /* fsFormat */])("string doesnt start with '%s'"),
            input: "string doesnt start with '%s'"
        }.formatFn(function (x) {
            return x;
        })(str), status.Text]);
    }, name, id);
}
function any(parsers) {
    var any_1 = function any_1(parsers_1, status) {
        any_1: while (true) {
            if (parsers_1.tail != null) {
                var matchValue = parsers_1.head.f(status);

                if (matchValue.tag === 1) {
                    parsers_1 = parsers_1.tail;
                    status = status;
                    continue any_1;
                } else {
                    return matchValue;
                }
            } else {
                return new ParserResult(1, ["Not found any of pattern", status.Text]);
            }
        }
    };

    var name = {
        formatFn: __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["a" /* fsFormat */])("any (%s)"),
        input: "any (%s)"
    }.formatFn(function (x) {
        return x;
    })(getNames(parsers));
    var id = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["f" /* newGuid */])();
    return new Parser(function (txt) {
        return any_1(parsers, txt);
    }, name, id);
}
var anyChar = function () {
    var id = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["f" /* newGuid */])();
    return new Parser(function (txt) {
        return any(function (list) {
            return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_12__packages_Fable_Core_fable_core_List__["c" /* map */])(function (ch) {
                return pChar(ch);
            }, list);
        }(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_13__packages_Fable_Core_fable_core_Seq__["d" /* toList */])(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_13__packages_Fable_Core_fable_core_Seq__["j" /* rangeChar */])("*", "z")))).f(txt);
    }, "anyChar", id);
}();
var anyNumber = function () {
    var id = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["f" /* newGuid */])();
    return new Parser(function (txt) {
        return any(function (list) {
            return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_12__packages_Fable_Core_fable_core_List__["c" /* map */])(function (ch) {
                return pChar(ch);
            }, list);
        }(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_13__packages_Fable_Core_fable_core_Seq__["d" /* toList */])(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_13__packages_Fable_Core_fable_core_Seq__["j" /* rangeChar */])("0", "9")))).f(txt);
    }, "anyNumber", id);
}();
function all(parser) {
    var all_1 = function all_1(status, agg) {
        all_1: while (true) {
            var matchValue = parser.f(status);

            if (matchValue.tag === 1) {
                if (agg.tail == null) {
                    return new ParserResult(1, [matchValue.data[0], status.Text]);
                } else {
                    return new ParserResult(0, [__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_12__packages_Fable_Core_fable_core_List__["d" /* reverse */])(agg), status]);
                }
            } else {
                var v = matchValue.data[0];
                var status_1 = matchValue.data[1];
                status = status_1;
                agg = new __WEBPACK_IMPORTED_MODULE_12__packages_Fable_Core_fable_core_List__["a" /* default */](v, agg);
                continue all_1;
            }
        }
    };

    var name = {
        formatFn: __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["a" /* fsFormat */])("all (%s)"),
        input: "all (%s)"
    }.formatFn(function (x) {
        return x;
    })(parser.name);
    var id = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["f" /* newGuid */])();
    return new Parser(function (txt) {
        return all_1(txt, new __WEBPACK_IMPORTED_MODULE_12__packages_Fable_Core_fable_core_List__["a" /* default */]());
    }, name, id);
}
function choice(parsers) {
    var name = {
        formatFn: __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["a" /* fsFormat */])("choice (%s)"),
        input: "choice (%s)"
    }.formatFn(function (x) {
        return x;
    })(getNames(parsers));
    var id = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["f" /* newGuid */])();
    return new Parser(function (status) {
        var matchValue = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_13__packages_Fable_Core_fable_core_Seq__["k" /* tryFind */])(function (r) {
            return Result.isParsed(r);
        }, __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_13__packages_Fable_Core_fable_core_Seq__["g" /* map */])(function (p) {
            return p.f(status);
        }, parsers));

        if (matchValue != null) {
            return matchValue;
        } else {
            return new ParserResult(1, [{
                formatFn: __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["a" /* fsFormat */])("failed to choice from %s"),
                input: "failed to choice from %s"
            }.formatFn(function (x) {
                return x;
            })(getNames(parsers)), status.Text]);
        }
    }, name, id);
}
function op_GreaterMinusGreater(a, b) {
    var name = a.name + ">->" + b.name;
    var id = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["f" /* newGuid */])();
    return new Parser(function ($var3) {
        return function () {
            var binder = function binder($var2) {
                return function (arg00_1) {
                    return b.f(arg00_1);
                }(function (tuple) {
                    return tuple[1];
                }($var2));
            };

            return function (bind_1) {
                return Result.bind(binder, bind_1);
            };
        }()(function (arg00) {
            return a.f(arg00);
        }($var3));
    }, name, id);
}
function op_LessMinusLess(a, b) {
    var name = a.name + "<-<" + b.name;
    var id = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["f" /* newGuid */])();
    return new Parser(function ($var4) {
        return function () {
            var binder = function binder(tupledArg) {
                return Result.map(function (_arg1) {
                    return tupledArg[0];
                }, b.f(tupledArg[1]));
            };

            return function (bind_1) {
                return Result.bind(binder, bind_1);
            };
        }()(function (arg00) {
            return a.f(arg00);
        }($var4));
    }, name, id);
}
function op_LessMinusGreater(a, b) {
    var name = a.name + "<->" + b.name;
    var id = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["f" /* newGuid */])();
    return new Parser(function ($var5) {
        return function () {
            var binder = function binder(tupledArg) {
                return Result.map(function (v2) {
                    return [tupledArg[0], v2];
                }, b.f(tupledArg[1]));
            };

            return function (bind_1) {
                return Result.bind(binder, bind_1);
            };
        }()(function (arg00) {
            return a.f(arg00);
        }($var5));
    }, name, id);
}
function op_BangBang(parser, name) {
    return new Parser(parser.parser, name, parser.id);
}
function op_EqualsEqualsGreater(a, map_1) {
    var name = {
        formatFn: __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["a" /* fsFormat */])("mapped-%s"),
        input: "mapped-%s"
    }.formatFn(function (x) {
        return x;
    })(a.name);
    var id = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["f" /* newGuid */])();
    return new Parser(function ($var6) {
        return function (v) {
            return Result.map(map_1, v);
        }(function (arg00) {
            return a.f(arg00);
        }($var6));
    }, name, id);
}
function between(a, b, a_) {
    return op_LessMinusLess(op_GreaterMinusGreater(a, b), a_);
}
function betweenChar(a, b, a_) {
    return op_LessMinusLess(op_GreaterMinusGreater(pChar(a), b), pChar(a_));
}
function betweenStr(a, b, a_) {
    return op_LessMinusLess(op_GreaterMinusGreater(pString(a), b), pString(a_));
}
function sepBy(sep, a) {
    var name = {
        formatFn: __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["a" /* fsFormat */])("sepBy (%A, %A)"),
        input: "sepBy (%A, %A)"
    }.formatFn(function (x) {
        return x;
    })(sep, a);
    var id = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["f" /* newGuid */])();
    return new Parser(function (status) {
        var matchValue = all(op_LessMinusLess(a, sep)).f(status);

        if (matchValue.tag === 1) {
            var matchValue_1 = a.f(status);

            if (matchValue_1.tag === 1) {
                return new ParserResult(1, [matchValue_1.data[0], status.Text]);
            } else {
                var v_ = matchValue_1.data[0];
                var status_1 = matchValue_1.data[1];
                return new ParserResult(0, [__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_12__packages_Fable_Core_fable_core_List__["b" /* ofArray */])([v_]), status_1]);
            }
        } else {
            var v = matchValue.data[0];
            var status_2 = matchValue.data[1];
            var matchValue_2 = a.f(status_2);

            if (matchValue_2.tag === 1) {
                return new ParserResult(0, [v, status_2]);
            } else {
                var v__1 = matchValue_2.data[0];
                var status_3 = matchValue_2.data[1];
                return new ParserResult(0, [__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_12__packages_Fable_Core_fable_core_List__["f" /* append */])(v, __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_12__packages_Fable_Core_fable_core_List__["b" /* ofArray */])([v__1])), status_3]);
            }
        }
    }, name, id);
}
function sepByChar(sep) {
    var sep_1 = pChar(sep);
    return function (a) {
        return sepBy(sep_1, a);
    };
}
var anyStr = op_EqualsEqualsGreater(all(anyChar), function ($var7) {
    return __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_array_from___default()($var7).join("");
});
var spaces1 = all(pChar(" "));
function spaces(txt) {
    return function () {
        var arg00_ = new __WEBPACK_IMPORTED_MODULE_12__packages_Fable_Core_fable_core_List__["a" /* default */]();
        return function (result) {
            return Result.defaultValue(arg00_, txt, result);
        };
    }()(all(pChar(" ")).f(txt));
}
function refl() {
    var b = null;
    var c = {
        contents: 0
    };
    return [function (x) {
        b = x;
    }, function () {
        var id = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["f" /* newGuid */])();
        return new Parser(function (status) {
            c.contents = c.contents + 1 | 0;

            if (c.contents < 1000) {
                var r = b.f(status);
                c.contents = c.contents - 1 | 0;
                return r;
            } else {
                return new ParserResult(1, ["stack-over-flow", status.Text]);
            }
        }, "refl", id);
    }()];
}
var pfloat = function () {
    var parse_1 = function parse_1(p) {
        return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_14__packages_Fable_Core_fable_core_Double__["a" /* parse */])(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_array_from___default()(p).join(""));
    };

    return op_GreaterTwiddleGreater(choice(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_12__packages_Fable_Core_fable_core_List__["b" /* ofArray */])([op_EqualsEqualsGreater(op_LessMinusGreater(op_LessMinusLess(all(anyNumber), pChar(".")), all(anyNumber)), function (tupledArg) {
        return parse_1(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_12__packages_Fable_Core_fable_core_List__["f" /* append */])(tupledArg[0], __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_12__packages_Fable_Core_fable_core_List__["f" /* append */])(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_12__packages_Fable_Core_fable_core_List__["b" /* ofArray */])(["."]), tupledArg[1])));
    }), op_EqualsEqualsGreater(op_LessMinusLess(all(anyNumber), pChar(".")), parse_1), op_EqualsEqualsGreater(all(anyNumber), parse_1)])), "pfloat");
}();
var pint = function () {
    var parse_1 = function parse_1(p) {
        return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_15__packages_Fable_Core_fable_core_Long__["b" /* fromString */])(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_array_from___default()(p).join(""), false);
    };

    return op_GreaterTwiddleGreater(op_EqualsEqualsGreater(all(anyNumber), parse_1), "pint");
}();
var pbool = op_GreaterTwiddleGreater(choice(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_12__packages_Fable_Core_fable_core_List__["b" /* ofArray */])([op_EqualsEqualsGreater(pString("true"), function (_arg1) {
    return true;
}), op_EqualsEqualsGreater(pString("false"), function (_arg2) {
    return false;
})])), "pbool");

/***/ }),
/* 152 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* unused harmony export Id */
/* unused harmony export AST */
/* unused harmony export exprAction */
/* unused harmony export expr */
/* unused harmony export quotedListCall */
/* unused harmony export listExpr */
/* unused harmony export RuntimeValue */
/* unused harmony export $7C$RNumbers$7C$_$7C$ */
/* unused harmony export $7C$EFunCall$7C$_$7C$ */
/* unused harmony export $7C$IsFuncDef$7C$_$7C$ */
/* unused harmony export evaluate */
/* harmony export (immutable) */ __webpack_exports__["a"] = run;
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_typeof__ = __webpack_require__(30);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_typeof___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_typeof__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck__ = __webpack_require__(9);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_createClass__ = __webpack_require__(10);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_createClass___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_createClass__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3__packages_Fable_Core_fable_core_Symbol__ = __webpack_require__(6);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4__packages_Fable_Core_fable_core_Util__ = __webpack_require__(1);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_5__packages_Fable_Core_fable_core_List__ = __webpack_require__(28);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_6__mparsec_fs__ = __webpack_require__(151);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_7__packages_Fable_Core_fable_core_Seq__ = __webpack_require__(3);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_8__packages_Fable_Core_fable_core_Map__ = __webpack_require__(29);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__ = __webpack_require__(40);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_10__packages_Fable_Core_fable_core_Result__ = __webpack_require__(60);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_11__packages_Fable_Core_fable_core_Comparer__ = __webpack_require__(38);















var Id = function () {
    function Id(tag, data) {
        __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck___default()(this, Id);

        this.tag = tag;
        this.data = data;
    }

    __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_createClass___default()(Id, [{
        key: __WEBPACK_IMPORTED_MODULE_3__packages_Fable_Core_fable_core_Symbol__["a" /* default */].reflection,
        value: function value() {
            return {
                type: "parser.Id",
                interfaces: ["FSharpUnion", "System.IEquatable", "System.IComparable"],
                cases: [["Id", "string"]]
            };
        }
    }, {
        key: "Equals",
        value: function Equals(other) {
            return this === other || this.tag === other.tag && __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__packages_Fable_Core_fable_core_Util__["c" /* equals */])(this.data, other.data);
        }
    }, {
        key: "CompareTo",
        value: function CompareTo(other) {
            return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__packages_Fable_Core_fable_core_Util__["d" /* compareUnions */])(this, other) | 0;
        }
    }]);

    return Id;
}();
__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_3__packages_Fable_Core_fable_core_Symbol__["b" /* setType */])("parser.Id", Id);
var AST = function (__exports) {
    var Expr = __exports.Expr = function () {
        function Expr(tag, data) {
            __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck___default()(this, Expr);

            this.tag = tag;
            this.data = data;
        }

        __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_createClass___default()(Expr, [{
            key: __WEBPACK_IMPORTED_MODULE_3__packages_Fable_Core_fable_core_Symbol__["a" /* default */].reflection,
            value: function value() {
                return {
                    type: "parser.AST.Expr",
                    interfaces: ["FSharpUnion", "System.IEquatable", "System.IComparable"],
                    cases: [["EString", "string"], ["ENumber", "number"], ["EId", Id], ["EList", __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__packages_Fable_Core_fable_core_Util__["e" /* makeGeneric */])(__WEBPACK_IMPORTED_MODULE_5__packages_Fable_Core_fable_core_List__["a" /* default */], {
                        T: Expr
                    })], ["EQuotedList", __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__packages_Fable_Core_fable_core_Util__["e" /* makeGeneric */])(__WEBPACK_IMPORTED_MODULE_5__packages_Fable_Core_fable_core_List__["a" /* default */], {
                        T: Expr
                    })]]
                };
            }
        }, {
            key: "Equals",
            value: function Equals(other) {
                return this === other || this.tag === other.tag && __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__packages_Fable_Core_fable_core_Util__["c" /* equals */])(this.data, other.data);
            }
        }, {
            key: "CompareTo",
            value: function CompareTo(other) {
                return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__packages_Fable_Core_fable_core_Util__["d" /* compareUnions */])(this, other) | 0;
            }
        }]);

        return Expr;
    }();

    __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_3__packages_Fable_Core_fable_core_Symbol__["b" /* setType */])("parser.AST.Expr", Expr);
    return __exports;
}({});
var patternInput_16 = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__mparsec_fs__["a" /* refl */])();
var exprAction = patternInput_16[0];
var expr = patternInput_16[1];
var quotedListCall = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__mparsec_fs__["b" /* op_EqualsEqualsGreater */])(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__mparsec_fs__["c" /* betweenStr */])("'(", __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__mparsec_fs__["d" /* sepBy */])(__WEBPACK_IMPORTED_MODULE_6__mparsec_fs__["e" /* spaces1 */], expr), ")"), function (arg0) {
    return new AST.Expr(4, arg0);
});
var listExpr = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__mparsec_fs__["b" /* op_EqualsEqualsGreater */])(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__mparsec_fs__["f" /* betweenChar */])("(", __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__mparsec_fs__["d" /* sepBy */])(__WEBPACK_IMPORTED_MODULE_6__mparsec_fs__["e" /* spaces1 */], expr), ")"), function (arg0) {
    return new AST.Expr(3, arg0);
});
exprAction(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__mparsec_fs__["g" /* choice */])(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__packages_Fable_Core_fable_core_List__["b" /* ofArray */])([__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__mparsec_fs__["b" /* op_EqualsEqualsGreater */])(__WEBPACK_IMPORTED_MODULE_6__mparsec_fs__["h" /* pfloat */], function (arg0) {
    return new AST.Expr(1, arg0);
}), __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__mparsec_fs__["i" /* op_GreaterTwiddleGreater */])(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__mparsec_fs__["b" /* op_EqualsEqualsGreater */])(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__mparsec_fs__["j" /* op_LessMinusLess */])(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__mparsec_fs__["k" /* op_GreaterMinusGreater */])(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__mparsec_fs__["l" /* pChar */])("\""), __WEBPACK_IMPORTED_MODULE_6__mparsec_fs__["m" /* anyStr */]), __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__mparsec_fs__["l" /* pChar */])("\"")), function (arg0_1) {
    return new AST.Expr(0, arg0_1);
}), "EString"), listExpr, quotedListCall, __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__mparsec_fs__["i" /* op_GreaterTwiddleGreater */])(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__mparsec_fs__["b" /* op_EqualsEqualsGreater */])(__WEBPACK_IMPORTED_MODULE_6__mparsec_fs__["m" /* anyStr */], function ($var1) {
    return new AST.Expr(2, new Id(0, $var1));
}), "EId")])));
var RuntimeValue = function () {
    function RuntimeValue(tag, data) {
        __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck___default()(this, RuntimeValue);

        this.tag = tag;
        this.data = data;
    }

    __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_createClass___default()(RuntimeValue, [{
        key: __WEBPACK_IMPORTED_MODULE_3__packages_Fable_Core_fable_core_Symbol__["a" /* default */].reflection,
        value: function value() {
            return {
                type: "parser.RuntimeValue",
                interfaces: ["FSharpUnion", "System.IEquatable", "System.IComparable"],
                cases: [["RNumber", "number"], ["RString", "string"], ["RUnit"], ["RFunction", __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__packages_Fable_Core_fable_core_Util__["e" /* makeGeneric */])(__WEBPACK_IMPORTED_MODULE_5__packages_Fable_Core_fable_core_List__["a" /* default */], {
                    T: Id
                }), AST.Expr]]
            };
        }
    }, {
        key: "Equals",
        value: function Equals(other) {
            return this === other || this.tag === other.tag && __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__packages_Fable_Core_fable_core_Util__["c" /* equals */])(this.data, other.data);
        }
    }, {
        key: "CompareTo",
        value: function CompareTo(other) {
            return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__packages_Fable_Core_fable_core_Util__["d" /* compareUnions */])(this, other) | 0;
        }
    }]);

    return RuntimeValue;
}();
__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_3__packages_Fable_Core_fable_core_Symbol__["b" /* setType */])("parser.RuntimeValue", RuntimeValue);

function _RNumbers___(exprs) {
    if (__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__packages_Fable_Core_fable_core_Seq__["a" /* forAll */])(function (_arg1) {
        return _arg1.tag === 1 ? true : false;
    }, exprs)) {
        return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__packages_Fable_Core_fable_core_List__["c" /* map */])(function (_arg2) {
            if (_arg2.tag === 1) {
                return _arg2.data;
            } else {
                throw new Error("impossible");
            }
        }, exprs);
    } else {
        return null;
    }
}



function _EFunCall___(_arg1) {
    var $var2 = _arg1.tag === 3 ? _arg1.data.tail != null ? _arg1.data.head.tag === 2 ? [0, _arg1.data.tail, _arg1.data.head.data] : [1] : [1] : [1];

    switch ($var2[0]) {
        case 0:
            return [$var2[2], $var2[1]];

        case 1:
            return null;
    }
}



function _IsFuncDef___(ast) {
    var isFunc = function isFunc(args, _arg1) {
        isFunc: while (true) {
            var $var3 = _arg1.tail != null ? _arg1.head.tag === 3 ? _arg1.tail.tail == null ? [0, _arg1.head] : [2] : _arg1.head.tag === 2 ? [1, _arg1.head.data, _arg1.tail] : [2] : [2];

            switch ($var3[0]) {
                case 0:
                    return new RuntimeValue(3, [__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__packages_Fable_Core_fable_core_List__["d" /* reverse */])(args), $var3[1]]);

                case 1:
                    args = new __WEBPACK_IMPORTED_MODULE_5__packages_Fable_Core_fable_core_List__["a" /* default */]($var3[1], args);
                    _arg1 = $var3[2];
                    continue isFunc;

                case 2:
                    return null;
            }
        }
    };

    var $var4 = ast.tag === 3 ? ast.data.tail != null ? ast.data.head.tag === 2 ? ast.data.head.data.data === "define" ? ast.data.tail.tail != null ? ast.data.tail.head.tag === 2 ? [0, ast.data.tail.tail, ast.data.tail.head.data] : [1] : [1] : [1] : [1] : [1] : [1];

    switch ($var4[0]) {
        case 0:
            return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__packages_Fable_Core_fable_core_Util__["f" /* defaultArg */])(isFunc(new __WEBPACK_IMPORTED_MODULE_5__packages_Fable_Core_fable_core_List__["a" /* default */](), $var4[1]), null, function (func) {
                return [$var4[2], func];
            });

        case 1:
            return null;
    }
}


function evaluate(environment, ast) {
    var _loop = function _loop() {
        var evaluate_ = function evaluate_(env, ast_1) {
            return evaluate(env, ast_1)[0];
        };

        var _EAllNumbers___ = function _EAllNumbers___(exprs) {
            var evaluated = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__packages_Fable_Core_fable_core_List__["c" /* map */])(function ($var5) {
                return evaluate_(environment, $var5);
            }, exprs);

            if (__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__packages_Fable_Core_fable_core_Seq__["a" /* forAll */])(function (_arg1) {
                return _arg1.tag === 0 ? true : false;
            }, evaluated)) {
                return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__packages_Fable_Core_fable_core_List__["e" /* choose */])(function (_arg2) {
                    return _arg2.tag === 0 ? _arg2.data : null;
                }, evaluated);
            } else {
                return null;
            }
        };

        var _IsFunCall___ = function _IsFunCall___(fname) {
            var matchValue = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__packages_Fable_Core_fable_core_Map__["a" /* tryFind */])(fname, environment);
            var $var6 = matchValue != null ? matchValue.tag === 3 ? [0, matchValue.data[0], matchValue.data[1]] : [1] : [1];

            switch ($var6[0]) {
                case 0:
                    return [$var6[1], $var6[2]];

                case 1:
                    return null;
            }
        };

        var $var7 = ast.tag === 1 ? [0, ast.data] : ast.tag === 0 ? [1, ast.data] : ast.tag === 2 ? environment.has(ast.data) ? [2, ast.data] : [3] : [3];

        switch ($var7[0]) {
            case 0:
                return {
                    v: [new RuntimeValue(0, $var7[1]), environment]
                };

            case 1:
                return {
                    v: [new RuntimeValue(1, $var7[1]), environment]
                };

            case 2:
                return {
                    v: [environment.get($var7[1]), environment]
                };

            case 3:
                var $var8 = void 0;

                var activePatternResult340 = _EFunCall___(ast);

                if (activePatternResult340 != null) {
                    if (activePatternResult340[0].data === "define") {
                        if (activePatternResult340[1].tail != null) {
                            if (activePatternResult340[1].head.tag === 2) {
                                if (activePatternResult340[1].tail.tail != null) {
                                    if (activePatternResult340[1].tail.tail.tail == null) {
                                        $var8 = [0, activePatternResult340[1].head.data, activePatternResult340[1].tail.head];
                                    } else {
                                        $var8 = [1];
                                    }
                                } else {
                                    $var8 = [1];
                                }
                            } else {
                                $var8 = [1];
                            }
                        } else {
                            $var8 = [1];
                        }
                    } else {
                        $var8 = [1];
                    }
                } else {
                    $var8 = [1];
                }

                switch ($var8[0]) {
                    case 0:
                        return {
                            v: [new RuntimeValue(2), __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__packages_Fable_Core_fable_core_Map__["b" /* add */])($var8[1], evaluate_(environment, $var8[2]), environment)]
                        };

                    case 1:
                        var activePatternResult339 = _IsFuncDef___(ast);

                        if (activePatternResult339 != null) {
                            return {
                                v: [new RuntimeValue(2), __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__packages_Fable_Core_fable_core_Map__["b" /* add */])(activePatternResult339[0], activePatternResult339[1], environment)]
                            };
                        } else {
                            var $var9 = void 0;

                            var activePatternResult337 = _EFunCall___(ast);

                            if (activePatternResult337 != null) {
                                if (activePatternResult337[0].data === "+") {
                                    var activePatternResult338 = _EAllNumbers___(activePatternResult337[1]);

                                    if (activePatternResult338 != null) {
                                        $var9 = [0, activePatternResult338];
                                    } else {
                                        $var9 = [1];
                                    }
                                } else {
                                    $var9 = [1];
                                }
                            } else {
                                $var9 = [1];
                            }

                            switch ($var9[0]) {
                                case 0:
                                    return {
                                        v: [new RuntimeValue(0, __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__packages_Fable_Core_fable_core_Seq__["b" /* reduce */])(function (x, y) {
                                            return x + y;
                                        }, $var9[1])), environment]
                                    };

                                case 1:
                                    var $var10 = void 0;

                                    var activePatternResult335 = _EFunCall___(ast);

                                    if (activePatternResult335 != null) {
                                        if (activePatternResult335[0].data === "-") {
                                            var activePatternResult336 = _EAllNumbers___(activePatternResult335[1]);

                                            if (activePatternResult336 != null) {
                                                $var10 = [0, activePatternResult336];
                                            } else {
                                                $var10 = [1];
                                            }
                                        } else {
                                            $var10 = [1];
                                        }
                                    } else {
                                        $var10 = [1];
                                    }

                                    switch ($var10[0]) {
                                        case 0:
                                            return {
                                                v: [new RuntimeValue(0, __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__packages_Fable_Core_fable_core_Seq__["b" /* reduce */])(function (x_1, y_1) {
                                                    return x_1 - y_1;
                                                }, $var10[1])), environment]
                                            };

                                        case 1:
                                            var $var11 = void 0;

                                            var activePatternResult333 = _EFunCall___(ast);

                                            if (activePatternResult333 != null) {
                                                if (activePatternResult333[0].data === "*") {
                                                    var activePatternResult334 = _EAllNumbers___(activePatternResult333[1]);

                                                    if (activePatternResult334 != null) {
                                                        $var11 = [0, activePatternResult334];
                                                    } else {
                                                        $var11 = [1];
                                                    }
                                                } else {
                                                    $var11 = [1];
                                                }
                                            } else {
                                                $var11 = [1];
                                            }

                                            switch ($var11[0]) {
                                                case 0:
                                                    return {
                                                        v: [new RuntimeValue(0, __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__packages_Fable_Core_fable_core_Seq__["b" /* reduce */])(function (x_2, y_2) {
                                                            return x_2 * y_2;
                                                        }, $var11[1])), environment]
                                                    };

                                                case 1:
                                                    var $var12 = void 0;

                                                    var activePatternResult331 = _EFunCall___(ast);

                                                    if (activePatternResult331 != null) {
                                                        if (activePatternResult331[0].data === "/") {
                                                            var activePatternResult332 = _EAllNumbers___(activePatternResult331[1]);

                                                            if (activePatternResult332 != null) {
                                                                $var12 = [0, activePatternResult332];
                                                            } else {
                                                                $var12 = [1];
                                                            }
                                                        } else {
                                                            $var12 = [1];
                                                        }
                                                    } else {
                                                        $var12 = [1];
                                                    }

                                                    switch ($var12[0]) {
                                                        case 0:
                                                            return {
                                                                v: [new RuntimeValue(0, __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__packages_Fable_Core_fable_core_Seq__["b" /* reduce */])(function (x_3, y_3) {
                                                                    return x_3 / y_3;
                                                                }, $var12[1])), environment]
                                                            };

                                                        case 1:
                                                            var $var13 = void 0;

                                                            var activePatternResult329 = _EFunCall___(ast);

                                                            if (activePatternResult329 != null) {
                                                                var activePatternResult330 = _IsFunCall___(activePatternResult329[0]);

                                                                if (activePatternResult330 != null) {
                                                                    if (function () {
                                                                        var func = activePatternResult330[1];
                                                                        var expr_1 = activePatternResult329[1];
                                                                        var args = activePatternResult330[0];
                                                                        return args.length === expr_1.length;
                                                                    }()) {
                                                                        $var13 = [0, activePatternResult330[0], activePatternResult329[1], activePatternResult330[1]];
                                                                    } else {
                                                                        $var13 = [1];
                                                                    }
                                                                } else {
                                                                    $var13 = [1];
                                                                }
                                                            } else {
                                                                $var13 = [1];
                                                            }

                                                            switch ($var13[0]) {
                                                                case 0:
                                                                    var environment_2 = function () {
                                                                        var folder = function folder(environment_1, tupledArg) {
                                                                            return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__packages_Fable_Core_fable_core_Map__["b" /* add */])(tupledArg[0], tupledArg[1], environment_1);
                                                                        };

                                                                        return function (list) {
                                                                            return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__packages_Fable_Core_fable_core_Seq__["c" /* fold */])(folder, environment, list);
                                                                        };
                                                                    }()(function (list2) {
                                                                        return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__packages_Fable_Core_fable_core_Seq__["d" /* toList */])(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__packages_Fable_Core_fable_core_Seq__["e" /* zip */])($var13[1], list2));
                                                                    }(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__packages_Fable_Core_fable_core_List__["c" /* map */])(function ($var14) {
                                                                        return evaluate_(environment, $var14);
                                                                    }, $var13[2])));

                                                                    environment = environment_2;
                                                                    ast = $var13[3];
                                                                    return "continue|evaluate";

                                                                case 1:
                                                                    return {
                                                                        v: {
                                                                            formatFn: __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["a" /* fsFormat */])("Unsupported AST %A"),
                                                                            input: "Unsupported AST %A"
                                                                        }.formatFn(function (x) {
                                                                            throw new Error(x);
                                                                        })(ast)
                                                                    };
                                                            }

                                                    }

                                            }

                                    }

                            }
                        }

                }

        }
    };

    evaluate: while (true) {
        var _ret = _loop();

        switch (_ret) {
            case "continue|evaluate":
                continue evaluate;

            default:
                if ((typeof _ret === "undefined" ? "undefined" : __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_typeof___default()(_ret)) === "object") return _ret.v;
        }
    }
}
function run(txt) {
    var run_1 = function run_1(txt_1, env) {
        var matchValue = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__mparsec_fs__["n" /* op_LessBangBangGreater */])(expr, txt_1);

        if (matchValue.tag === 1) {
            return new __WEBPACK_IMPORTED_MODULE_10__packages_Fable_Core_fable_core_Result__["a" /* default */](1, matchValue.data[0]);
        } else {
            var parsed = matchValue.data[0];
            return new __WEBPACK_IMPORTED_MODULE_10__packages_Fable_Core_fable_core_Result__["a" /* default */](0, evaluate(env, parsed));
        }
    };

    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__packages_Fable_Core_fable_core_Seq__["c" /* fold */])(function (env_1, code) {
        return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_10__packages_Fable_Core_fable_core_Result__["b" /* bind */])(function (tupledArg) {
            ({
                formatFn: __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["a" /* fsFormat */])("%A"),
                input: "%A"
            }).formatFn(function (x) {
                console.log(x);
            })(tupledArg[0]);
            return run_1(code, tupledArg[1]);
        }, env_1);
    }, new __WEBPACK_IMPORTED_MODULE_10__packages_Fable_Core_fable_core_Result__["a" /* default */](0, [new RuntimeValue(2), __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__packages_Fable_Core_fable_core_Map__["c" /* create */])(null, new __WEBPACK_IMPORTED_MODULE_11__packages_Fable_Core_fable_core_Comparer__["a" /* default */](function (x, y) {
        return x.CompareTo(y);
    }))]), __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["b" /* split */])(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__packages_Fable_Core_fable_core_String__["c" /* replace */])(txt, "\r\n", "\n"), "\n"));
}
run("(define add2 z (+ 2 z))\n(define id i (+ 2 z))\n(add2 5)\n(add2 7)\n(define x (+ 1 2 3 (add2 10)))\n(define marcin \"Marcin\")\n(+ x)");

/***/ })
/******/ ]);
//# sourceMappingURL=bundle.js.map