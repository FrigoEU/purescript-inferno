(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
'use strict';

module.exports = require('inferno/dist/inferno-dom');
},{"inferno/dist/inferno-dom":2}],2:[function(require,module,exports){
/*!
 * inferno-dom v1.0.0-alpha10
 * (c) 2016 Dominic Gannaway
 * Released under the MIT License.
 */
(function (global, factory) {
	typeof exports === 'object' && typeof module !== 'undefined' ? module.exports = factory() :
	typeof define === 'function' && define.amd ? define(factory) :
	(global.InfernoDOM = factory());
}(this, (function () { 'use strict';

var Lifecycle = function Lifecycle() {
    this._listeners = [];
};
Lifecycle.prototype.addListener = function addListener (callback) {
    this._listeners.push(callback);
};
Lifecycle.prototype.trigger = function trigger () {
        var this$1 = this;

    for (var i = 0; i < this._listeners.length; i++) {
        this$1._listeners[i]();
    }
};

var NO_OP = '$NO_OP';
var ERROR_MSG = 'a runtime error occured! Use Inferno in development environment to find the error.';
var isBrowser = typeof window !== 'undefined' && window.document;
function isArray(obj) {
    return obj instanceof Array;
}
function isStatefulComponent(o) {
    var component = o.component;
    return !isUndefined(component.prototype) && !isUndefined(component.prototype.render);
}
function isStringOrNumber(obj) {
    return isString(obj) || isNumber(obj);
}
function isNullOrUndef(obj) {
    return isUndefined(obj) || isNull(obj);
}
function isInvalid(obj) {
    return isNull(obj) || obj === false || isTrue(obj) || isUndefined(obj);
}
function isFunction(obj) {
    return typeof obj === 'function';
}
function isAttrAnEvent(attr) {
    return attr[0] === 'o' && attr[1] === 'n' && attr.length > 3;
}
function isString(obj) {
    return typeof obj === 'string';
}
function isNumber(obj) {
    return typeof obj === 'number';
}
function isNull(obj) {
    return obj === null;
}
function isTrue(obj) {
    return obj === true;
}
function isUndefined(obj) {
    return obj === undefined;
}
function isObject(o) {
    return typeof o === 'object';
}
function throwError(message) {
    if (!message) {
        message = ERROR_MSG;
    }
    throw new Error(("Inferno Error: " + message));
}
var EMPTY_OBJ = {};

var ValueTypes = {
    CHILDREN: 1,
    PROP_CLASS_NAME: 2,
    PROP_STYLE: 3,
    PROP_DATA: 4,
    PROP_REF: 5,
    PROP_SPREAD: 6,
    PROP_VALUE: 7,
    PROP: 8
};
var ChildrenTypes = {
    NON_KEYED: 1,
    KEYED: 2,
    NODE: 3,
    TEXT: 4,
    UNKNOWN: 5
};
var NodeTypes = {
    ELEMENT: 1,
    OPT_ELEMENT: 2,
    TEXT: 3,
    FRAGMENT: 4,
    OPT_BLUEPRINT: 5,
    COMPONENT: 6,
    PLACEHOLDER: 7
};
function isUnknownChildrenType(o) {
    return o === ChildrenTypes.UNKNOWN;
}
function isKeyedListChildrenType(o) {
    return o === ChildrenTypes.KEYED;
}
function isNonKeyedListChildrenType(o) {
    return o === ChildrenTypes.NON_KEYED;
}
function isTextChildrenType(o) {
    return o === ChildrenTypes.TEXT;
}
function isNodeChildrenType(o) {
    return o === ChildrenTypes.NODE;
}

function createVComponent(component, props, key, hooks, ref) {
    return {
        component: component,
        dom: null,
        hooks: hooks || null,
        instance: null,
        key: key,
        props: props,
        ref: ref || null,
        type: NodeTypes.COMPONENT
    };
}
function createVText(text) {
    return {
        dom: null,
        text: text,
        type: NodeTypes.TEXT
    };
}
function createVElement(tag, props, children, key, ref, childrenType) {
    return {
        children: children,
        childrenType: childrenType || ChildrenTypes.UNKNOWN,
        dom: null,
        key: key,
        props: props,
        ref: ref || null,
        tag: tag,
        type: NodeTypes.ELEMENT
    };
}
function createVFragment(children, childrenType) {
    return {
        children: children,
        childrenType: childrenType || ChildrenTypes.UNKNOWN,
        dom: null,
        pointer: null,
        type: NodeTypes.FRAGMENT
    };
}
function createVPlaceholder() {
    return {
        dom: null,
        type: NodeTypes.PLACEHOLDER
    };
}
function isVElement(o) {
    return o.type === NodeTypes.ELEMENT;
}
function isOptVElement(o) {
    return o.type === NodeTypes.OPT_ELEMENT;
}
function isVComponent(o) {
    return o.type === NodeTypes.COMPONENT;
}
function isVText(o) {
    return o.type === NodeTypes.TEXT;
}
function isVFragment(o) {
    return o.type === NodeTypes.FRAGMENT;
}
function isVPlaceholder(o) {
    return o.type === NodeTypes.PLACEHOLDER;
}
function isVNode(o) {
    return !isUndefined(o.type);
}

var recyclingEnabled = true;
var vComponentPools = new Map();
function recycleOptVElement(optVElement, lifecycle, context, isSVG, shallowUnmount) {
    var bp = optVElement.bp;
    var key = optVElement.key;
    var pool = key === null ? bp.pools.nonKeyed : bp.pools.keyed.get(key);
    if (!isUndefined(pool)) {
        var recycledOptVElement = pool.pop();
        if (!isUndefined(recycledOptVElement)) {
            patchOptVElement(recycledOptVElement, optVElement, null, lifecycle, context, isSVG, shallowUnmount);
            return optVElement.dom;
        }
    }
    return null;
}
function poolOptVElement(optVElement) {
    var bp = optVElement.bp;
    var key = optVElement.key;
    var pools = bp.pools;
    if (isNull(key)) {
        pools.nonKeyed.push(optVElement);
    }
    else {
        var pool = pools.keyed.get(key);
        if (isUndefined(pool)) {
            pool = [];
            pools.keyed.set(key, pool);
        }
        pool.push(optVElement);
    }
}
function recycleVComponent(vComponent, lifecycle, context, isSVG, shallowUnmount) {
    var component = vComponent.component;
    var key = vComponent.key;
    var pools = vComponentPools.get(component);
    if (!isUndefined(pools)) {
        var pool = key === null ? pools.nonKeyed : pools.keyed.get(key);
        if (!isUndefined(pool)) {
            var recycledVComponent = pool.pop();
            if (!isUndefined(recycledVComponent)) {
                var failed = patchVComponent(recycledVComponent, vComponent, null, lifecycle, context, isSVG, shallowUnmount);
                if (!failed) {
                    return vComponent.dom;
                }
            }
        }
    }
    return null;
}
function poolVComponent(vComponent) {
    var component = vComponent.component;
    var key = vComponent.key;
    var pools = vComponentPools.get(component);
    if (isUndefined(pools)) {
        pools = {
            nonKeyed: [],
            keyed: new Map()
        };
        vComponentPools.set(component, pools);
    }
    if (isNull(key)) {
        pools.nonKeyed.push(vComponent);
    }
    else {
        var pool = pools.keyed.get(key);
        if (isUndefined(pool)) {
            pool = [];
            pools.keyed.set(key, pool);
        }
        pool.push(vComponent);
    }
}

function unmount(input, parentDom, lifecycle, canRecycle, shallowUnmount) {
    if (!isInvalid(input)) {
        if (isOptVElement(input)) {
            unmountOptVElement(input, parentDom, lifecycle, canRecycle, shallowUnmount);
        }
        else if (isVComponent(input)) {
            unmountVComponent(input, parentDom, lifecycle, canRecycle, shallowUnmount);
        }
        else if (isVElement(input)) {
            unmountVElement(input, parentDom, lifecycle, shallowUnmount);
        }
        else if (isVFragment(input)) {
            unmountVFragment(input, parentDom, true, lifecycle, shallowUnmount);
        }
        else if (isVText(input)) {
            unmountVText(input, parentDom);
        }
        else if (isVPlaceholder(input)) {
            unmountVPlaceholder(input, parentDom);
        }
    }
}
function unmountVPlaceholder(vPlaceholder, parentDom) {
    if (parentDom) {
        removeChild(parentDom, vPlaceholder.dom);
    }
}
function unmountVText(vText, parentDom) {
    if (parentDom) {
        removeChild(parentDom, vText.dom);
    }
}
function unmountOptVElement(optVElement, parentDom, lifecycle, canRecycle, shallowUnmount) {
    var bp = optVElement.bp;
    var bp0 = bp.v0;
    if (!shallowUnmount) {
        if (!isNull(bp0)) {
            unmountOptVElementValue(optVElement, bp0, optVElement.v0, lifecycle, shallowUnmount);
            var bp1 = bp.v1;
            if (!isNull(bp1)) {
                unmountOptVElementValue(optVElement, bp1, optVElement.v1, lifecycle, shallowUnmount);
                var bp2 = bp.v2;
                if (!isNull(bp2)) {
                    unmountOptVElementValue(optVElement, bp2, optVElement.v2, lifecycle, shallowUnmount);
                }
            }
        }
    }
    if (!isNull(parentDom)) {
        parentDom.removeChild(optVElement.dom);
    }
    if (recyclingEnabled && (parentDom || canRecycle)) {
        poolOptVElement(optVElement);
    }
}
function unmountOptVElementValue(optVElement, valueType, value, lifecycle, shallowUnmount) {
    switch (valueType) {
        case ValueTypes.CHILDREN:
            unmountChildren(value, lifecycle, shallowUnmount);
            break;
        case ValueTypes.PROP_REF:
            unmountRef(value);
            break;
        case ValueTypes.PROP_SPREAD:
            unmountProps(value, lifecycle);
            break;
    }
}
function unmountVFragment(vFragment, parentDom, removePointer, lifecycle, shallowUnmount) {
    var children = vFragment.children;
    var childrenLength = children.length;
    var pointer = vFragment.pointer;
    if (!shallowUnmount && childrenLength > 0) {
        for (var i = 0; i < childrenLength; i++) {
            var child = children[i];
            if (isVFragment(child)) {
                unmountVFragment(child, parentDom, true, lifecycle, false);
            }
            else {
                unmount(child, parentDom, lifecycle, false, shallowUnmount);
            }
        }
    }
    if (parentDom && removePointer) {
        removeChild(parentDom, pointer);
    }
}
function unmountVComponent(vComponent, parentDom, lifecycle, canRecycle, shallowUnmount) {
    if (!shallowUnmount) {
        var instance = vComponent.instance;
        var instanceHooks = null;
        var instanceChildren = null;
        vComponent.unmounted = true;
        if (!isNullOrUndef(instance)) {
            var ref = vComponent.ref;
            if (ref) {
                ref(null);
            }
            instanceHooks = instance.hooks;
            instanceChildren = instance.children;
            if (instance.render !== undefined) {
                instance.componentWillUnmount();
                instance._unmounted = true;
                componentToDOMNodeMap.delete(instance);
                unmount(instance._lastInput, null, lifecycle, false, shallowUnmount);
            }
            else {
                unmount(instance, null, lifecycle, false, shallowUnmount);
            }
        }
        var hooks = vComponent.hooks || instanceHooks;
        if (!isNullOrUndef(hooks)) {
            if (!isNullOrUndef(hooks.onComponentWillUnmount)) {
                hooks.onComponentWillUnmount(hooks);
            }
        }
    }
    if (parentDom) {
        removeChild(parentDom, vComponent.dom);
    }
    if (recyclingEnabled && (parentDom || canRecycle)) {
        poolVComponent(vComponent);
    }
}
function unmountVElement(vElement, parentDom, lifecycle, shallowUnmount) {
    var dom = vElement.dom;
    var ref = vElement.ref;
    if (!shallowUnmount) {
        if (ref) {
            unmountRef(ref);
        }
        var children = vElement.children;
        if (!isNullOrUndef(children)) {
            unmountChildren(children, lifecycle, shallowUnmount);
        }
    }
    if (parentDom) {
        removeChild(parentDom, dom);
    }
}
function unmountChildren(children, lifecycle, shallowUnmount) {
    if (isArray(children)) {
        for (var i = 0; i < children.length; i++) {
            var child = children[i];
            if (isObject(child)) {
                unmount(child, null, lifecycle, false, shallowUnmount);
            }
        }
    }
    else if (isObject(children)) {
        unmount(children, null, lifecycle, false, shallowUnmount);
    }
}
function unmountRef(ref) {
    if (isFunction(ref)) {
        ref(null);
    }
    else {
        if (isInvalid(ref)) {
            return;
        }
        if ("development" !== 'production') {
            throwError('string "refs" are not supported in Inferno 0.8+. Use callback "refs" instead.');
        }
        throwError();
    }
}
function unmountProps(props, lifecycle) {
    for (var prop in props) {
        var value = props[prop];
        if (prop === 'ref') {
            unmountRef(value);
        }
    }
}

function constructDefaults(string, object, value) {
    /* eslint no-return-assign: 0 */
    string.split(',').forEach(function (i) { return object[i] = value; });
}
var xlinkNS = 'http://www.w3.org/1999/xlink';
var xmlNS = 'http://www.w3.org/XML/1998/namespace';
var svgNS = 'http://www.w3.org/2000/svg';
var strictProps = {};
var booleanProps = {};
var namespaces = {};
var isUnitlessNumber = {};
constructDefaults('xlink:href,xlink:arcrole,xlink:actuate,xlink:role,xlink:titlef,xlink:type', namespaces, xlinkNS);
constructDefaults('xml:base,xml:lang,xml:space', namespaces, xmlNS);
constructDefaults('volume,value', strictProps, true);
constructDefaults('muted,scoped,loop,open,checked,default,capture,disabled,selected,readonly,multiple,required,autoplay,controls,seamless,reversed,allowfullscreen,novalidate', booleanProps, true);
constructDefaults('animationIterationCount,borderImageOutset,borderImageSlice,borderImageWidth,boxFlex,boxFlexGroup,boxOrdinalGroup,columnCount,flex,flexGrow,flexPositive,flexShrink,flexNegative,flexOrder,gridRow,gridColumn,fontWeight,lineClamp,lineHeight,opacity,order,orphans,tabSize,widows,zIndex,zoom,fillOpacity,floodOpacity,stopOpacity,strokeDasharray,strokeDashoffset,strokeMiterlimit,strokeOpacity,strokeWidth,', isUnitlessNumber, true);

function replaceLastChildAndUnmount(lastInput, nextInput, parentDom, lifecycle, context, isSVG, shallowUnmount) {
    replaceChild(parentDom, mount(nextInput, null, lifecycle, context, isSVG, shallowUnmount), lastInput.dom);
    unmount(lastInput, null, lifecycle, false, shallowUnmount);
}
function patch(lastInput, nextInput, parentDom, lifecycle, context, isSVG, shallowUnmount) {
    if (lastInput !== nextInput) {
        if (isOptVElement(nextInput)) {
            if (isOptVElement(lastInput)) {
                patchOptVElement(lastInput, nextInput, parentDom, lifecycle, context, isSVG, shallowUnmount);
            }
            else {
                replaceChild(parentDom, mountOptVElement(nextInput, null, lifecycle, context, isSVG, shallowUnmount), lastInput.dom);
                unmount(lastInput, null, lifecycle, false, shallowUnmount);
            }
        }
        else if (isOptVElement(lastInput)) {
            replaceLastChildAndUnmount(lastInput, nextInput, parentDom, lifecycle, context, isSVG, shallowUnmount);
        }
        else if (isVComponent(nextInput)) {
            if (isVComponent(lastInput)) {
                patchVComponent(lastInput, nextInput, parentDom, lifecycle, context, isSVG, shallowUnmount);
            }
            else {
                replaceChild(parentDom, mountVComponent(nextInput, null, lifecycle, context, isSVG, shallowUnmount), lastInput.dom);
                unmount(lastInput, null, lifecycle, false, shallowUnmount);
            }
        }
        else if (isVComponent(lastInput)) {
            replaceLastChildAndUnmount(lastInput, nextInput, parentDom, lifecycle, context, isSVG, shallowUnmount);
        }
        else if (isVElement(nextInput)) {
            if (isVElement(lastInput)) {
                patchVElement(lastInput, nextInput, parentDom, lifecycle, context, isSVG, shallowUnmount);
            }
            else {
                replaceChild(parentDom, mountVElement(nextInput, null, lifecycle, context, isSVG, shallowUnmount), lastInput.dom);
                unmount(lastInput, null, lifecycle, false, shallowUnmount);
            }
        }
        else if (isVFragment(nextInput)) {
            if (isVFragment(lastInput)) {
                patchVFragment(lastInput, nextInput, parentDom, lifecycle, context, isSVG, shallowUnmount);
            }
            else {
                replaceChild(parentDom, mountVFragment(nextInput, null, lifecycle, context, isSVG, shallowUnmount), lastInput.dom);
                unmount(lastInput, null, lifecycle, false, shallowUnmount);
            }
        }
        else if (isVFragment(lastInput)) {
            replaceVListWithNode(parentDom, lastInput, mount(nextInput, null, lifecycle, context, isSVG, shallowUnmount), lifecycle, shallowUnmount);
        }
        else if (isVElement(lastInput)) {
            replaceLastChildAndUnmount(lastInput, nextInput, parentDom, lifecycle, context, isSVG, shallowUnmount);
        }
        else if (isVText(nextInput)) {
            if (isVText(lastInput)) {
                patchVText(lastInput, nextInput);
            }
            else {
                replaceChild(parentDom, mountVText(nextInput, null), lastInput.dom);
                unmount(lastInput, null, lifecycle, false, shallowUnmount);
            }
        }
        else if (isVText(lastInput)) {
            replaceChild(parentDom, mount(nextInput, null, lifecycle, context, isSVG, shallowUnmount), lastInput.dom);
        }
        else if (isVPlaceholder(nextInput)) {
            if (isVPlaceholder(lastInput)) {
                patchVPlaceholder(lastInput, nextInput);
            }
            else {
                replaceChild(parentDom, mountVPlaceholder(nextInput, null), lastInput.dom);
                unmount(lastInput, null, lifecycle, false, shallowUnmount);
            }
        }
        else if (isVPlaceholder(lastInput)) {
            replaceChild(parentDom, mount(nextInput, null, lifecycle, context, isSVG, shallowUnmount), lastInput.dom);
        }
        else {
            if ("development" !== 'production') {
                throwError('bad input argument called on patch(). Input argument may need normalising.');
            }
            throwError();
        }
    }
}
function patchVElement(lastVElement, nextVElement, parentDom, lifecycle, context, isSVG, shallowUnmount) {
    var nextTag = nextVElement.tag;
    var lastTag = lastVElement.tag;
    if (nextTag === 'svg') {
        isSVG = true;
    }
    if (lastTag !== nextTag) {
        replaceWithNewNode(lastVElement, nextVElement, parentDom, lifecycle, context, isSVG, shallowUnmount);
    }
    else {
        var dom = lastVElement.dom;
        var lastProps = lastVElement.props;
        var nextProps = nextVElement.props;
        var lastChildren = lastVElement.children;
        var nextChildren = nextVElement.children;
        nextVElement.dom = dom;
        if (lastChildren !== nextChildren) {
            var lastChildrenType = lastVElement.childrenType;
            var nextChildrenType = nextVElement.childrenType;
            if (lastChildrenType === nextChildrenType) {
                patchChildren(lastChildrenType, lastChildren, nextChildren, dom, lifecycle, context, isSVG, shallowUnmount);
            }
            else {
                patchChildrenWithUnknownType(lastChildren, nextChildren, dom, lifecycle, context, isSVG, shallowUnmount);
            }
        }
        if (lastProps !== nextProps) {
            var formValue = patchProps(lastProps, nextProps, dom, shallowUnmount);
            if (nextTag === 'select') {
                formSelectValue(dom, formValue);
            }
        }
    }
}
function patchOptVElement(lastOptVElement, nextOptVElement, parentDom, lifecycle, context, isSVG, shallowUnmount) {
    var dom = lastOptVElement.dom;
    var lastBp = lastOptVElement.bp;
    var nextBp = nextOptVElement.bp;
    nextOptVElement.dom = dom;
    if (lastBp !== nextBp) {
        var newDom = mountOptVElement(nextOptVElement, null, lifecycle, context, isSVG, shallowUnmount);
        replaceChild(parentDom, newDom, dom);
        unmount(lastOptVElement, null, lifecycle, true, shallowUnmount);
    }
    else {
        var bp0 = nextBp.v0;
        var tag = nextBp.staticVElement.tag;
        var ignoreDiff = false;
        if (tag === 'svg') {
            isSVG = true;
        }
        else if (tag === 'input') {
            // input elements are problematic due to the large amount of internal state that hold
            // so instead of making lots of assumptions, we instead reset common values and re-apply
            // the the patching each time
            resetFormInputProperties(dom);
            ignoreDiff = true;
        }
        else if (tag === 'select') {
        }
        else if (tag === 'textarea') {
            // textarea elements are like input elements, except they have sligthly less internal state to
            // worry about
            ignoreDiff = true;
        }
        if (!isNull(bp0)) {
            var lastV0 = lastOptVElement.v0;
            var nextV0 = nextOptVElement.v0;
            var bp1 = nextBp.v1;
            if (lastV0 !== nextV0 || ignoreDiff) {
                patchOptVElementValue(bp0, lastV0, nextV0, nextBp.d0, dom, lifecycle, context, isSVG, shallowUnmount);
            }
            if (!isNull(bp1)) {
                var lastV1 = lastOptVElement.v1;
                var nextV1 = nextOptVElement.v1;
                var bp2 = nextBp.v2;
                if (lastV1 !== nextV1 || ignoreDiff) {
                    patchOptVElementValue(bp1, lastV1, nextV1, nextBp.d1, dom, lifecycle, context, isSVG, shallowUnmount);
                }
                if (!isNull(bp2)) {
                    var lastV2 = lastOptVElement.v2;
                    var nextV2 = nextOptVElement.v2;
                    var bp3 = nextBp.v3;
                    if (lastV2 !== nextV2 || ignoreDiff) {
                        patchOptVElementValue(bp2, lastV2, nextV2, nextBp.d2, dom, lifecycle, context, isSVG, shallowUnmount);
                    }
                    if (!isNull(bp3)) {
                        var d3 = nextBp.d3;
                        var lastV3s = lastOptVElement.v3;
                        var nextV3s = nextOptVElement.v3;
                        for (var i = 0; i < lastV3s.length; i++) {
                            var lastV3 = lastV3s[i];
                            var nextV3 = nextV3s[i];
                            if (lastV3 !== nextV3 || ignoreDiff) {
                                patchOptVElementValue(bp3[i], lastV3, nextV3, d3[i], dom, lifecycle, context, isSVG, shallowUnmount);
                            }
                        }
                    }
                }
            }
        }
        if (tag === 'select') {
            formSelectValue(dom, getPropFromOptElement(nextOptVElement, ValueTypes.PROP_VALUE));
        }
    }
}
function patchOptVElementValue(valueType, lastValue, nextValue, descriptor, dom, lifecycle, context, isSVG, shallowUnmount) {
    switch (valueType) {
        case ValueTypes.CHILDREN:
            patchChildren(descriptor, lastValue, nextValue, dom, lifecycle, context, isSVG, shallowUnmount);
            break;
        case ValueTypes.PROP_CLASS_NAME:
            if (isNullOrUndef(nextValue)) {
                dom.removeAttribute('class');
            }
            else {
                if (isSVG) {
                    dom.setAttribute('class', nextValue);
                }
                else {
                    dom.className = nextValue;
                }
            }
            break;
        case ValueTypes.PROP_DATA:
            dom.dataset[descriptor] = nextValue;
            break;
        case ValueTypes.PROP_STYLE:
            patchStyle(lastValue, nextValue, dom);
            break;
        case ValueTypes.PROP_VALUE:
            dom.value = isNullOrUndef(nextValue) ? '' : nextValue;
            break;
        case ValueTypes.PROP:
            patchProp(descriptor, lastValue, nextValue, dom);
            break;
        case ValueTypes.PROP_SPREAD:
            patchProps(lastValue, nextValue, dom, shallowUnmount);
            break;
    }
}
function patchChildren(childrenType, lastChildren, nextChildren, parentDom, lifecycle, context, isSVG, shallowUnmount) {
    if (isTextChildrenType(childrenType)) {
        updateTextContent(parentDom, nextChildren);
    }
    else if (isNodeChildrenType(childrenType)) {
        patch(lastChildren, nextChildren, parentDom, lifecycle, context, isSVG, shallowUnmount);
    }
    else if (isKeyedListChildrenType(childrenType)) {
        patchKeyedChildren(lastChildren, nextChildren, parentDom, lifecycle, context, isSVG, null, shallowUnmount);
    }
    else if (isNonKeyedListChildrenType(childrenType)) {
        patchNonKeyedChildren(lastChildren, nextChildren, parentDom, lifecycle, context, isSVG, null, false, shallowUnmount);
    }
    else if (isUnknownChildrenType(childrenType)) {
        patchChildrenWithUnknownType(lastChildren, nextChildren, parentDom, lifecycle, context, isSVG, shallowUnmount);
    }
    else {
        if ("development" !== 'production') {
            throwError('bad childrenType value specified when attempting to patchChildren.');
        }
        throwError();
    }
}
function patchChildrenWithUnknownType(lastChildren, nextChildren, parentDom, lifecycle, context, isSVG, shallowUnmount) {
    if (isInvalid(nextChildren)) {
        if (!isInvalid(lastChildren)) {
            if (isVNode(lastChildren)) {
                unmount(lastChildren, parentDom, lifecycle, true, shallowUnmount);
            }
            else {
                removeAllChildren(parentDom, lastChildren, lifecycle, shallowUnmount);
            }
        }
    }
    else if (isInvalid(lastChildren)) {
        if (isStringOrNumber(nextChildren)) {
            setTextContent(parentDom, nextChildren);
        }
        else if (!isInvalid(nextChildren)) {
            if (isArray(nextChildren)) {
                mountArrayChildrenWithoutType(nextChildren, parentDom, lifecycle, context, isSVG, shallowUnmount);
            }
            else {
                mount(nextChildren, parentDom, lifecycle, context, isSVG, shallowUnmount);
            }
        }
    }
    else if (isVNode(lastChildren) && isVNode(nextChildren)) {
        patch(lastChildren, nextChildren, parentDom, lifecycle, context, isSVG, shallowUnmount);
    }
    else if (isStringOrNumber(nextChildren)) {
        if (isStringOrNumber(lastChildren)) {
            updateTextContent(parentDom, nextChildren);
        }
        else {
            setTextContent(parentDom, nextChildren);
        }
    }
    else if (isStringOrNumber(lastChildren)) {
        var child = normalise(lastChildren);
        child.dom = parentDom.firstChild;
        patchChildrenWithUnknownType(child, nextChildren, parentDom, lifecycle, context, isSVG, shallowUnmount);
    }
    else if (isArray(nextChildren)) {
        if (isArray(lastChildren)) {
            nextChildren.complex = lastChildren.complex;
            if (isKeyed(lastChildren, nextChildren)) {
                patchKeyedChildren(lastChildren, nextChildren, parentDom, lifecycle, context, isSVG, null, shallowUnmount);
            }
            else {
                patchNonKeyedChildren(lastChildren, nextChildren, parentDom, lifecycle, context, isSVG, null, true, shallowUnmount);
            }
        }
        else {
            patchNonKeyedChildren([lastChildren], nextChildren, parentDom, lifecycle, context, isSVG, null, true, shallowUnmount);
        }
    }
    else if (isArray(lastChildren)) {
        patchNonKeyedChildren(lastChildren, [nextChildren], parentDom, lifecycle, context, isSVG, null, true, shallowUnmount);
    }
    else {
        if ("development" !== 'production') {
            throwError('bad input argument called on patchChildrenWithUnknownType(). Input argument may need normalising.');
        }
        throwError();
    }
}
function patchVComponent(lastVComponent, nextVComponent, parentDom, lifecycle, context, isSVG, shallowUnmount) {
    var lastComponent = lastVComponent.component;
    var nextComponent = nextVComponent.component;
    var nextProps = nextVComponent.props || {};
    if (lastComponent !== nextComponent) {
        if (isStatefulComponent(nextVComponent)) {
            var defaultProps = nextComponent.defaultProps;
            if (!isUndefined(defaultProps)) {
                nextVComponent.props = copyPropsTo(defaultProps, nextProps);
            }
            var lastInstance = lastVComponent.instance;
            var nextInstance = createStatefulComponentInstance(nextComponent, nextProps, context, isSVG);
            // we use || lastInstance because stateless components store their lastInstance
            var lastInput = lastInstance._lastInput || lastInstance;
            var nextInput = nextInstance._lastInput;
            var ref = nextVComponent.ref;
            nextInstance._vComponent = nextVComponent;
            nextVComponent.instance = nextInstance;
            patch(lastInput, nextInput, parentDom, lifecycle, nextInstance._childContext, isSVG, true);
            mountStatefulComponentCallbacks(ref, nextInstance, lifecycle);
            nextVComponent.dom = nextInput.dom;
            componentToDOMNodeMap.set(nextInstance, nextInput.dom);
        }
        else {
            var lastInput$1 = lastVComponent.instance._lastInput || lastVComponent.instance;
            var nextInput$1 = createStatelessComponentInput(nextComponent, nextProps, context);
            patch(lastInput$1, nextInput$1, parentDom, lifecycle, context, isSVG, true);
            var dom = nextVComponent.dom = nextInput$1.dom;
            nextVComponent.instance = nextInput$1;
            mountStatelessComponentCallbacks(nextVComponent.hooks, dom, lifecycle);
        }
        unmount(lastVComponent, null, lifecycle, false, shallowUnmount);
    }
    else {
        if (isStatefulComponent(nextVComponent)) {
            var instance = lastVComponent.instance;
            if (instance._unmounted) {
                if (isNull(parentDom)) {
                    return true;
                }
                replaceChild(parentDom, mountVComponent(nextVComponent, null, lifecycle, context, isSVG, shallowUnmount), lastVComponent.dom);
            }
            else {
                var defaultProps$1 = nextComponent.defaultProps;
                var lastProps = instance.props;
                if (!isUndefined(defaultProps$1)) {
                    copyPropsTo(lastProps, nextProps);
                    nextVComponent.props = nextProps;
                }
                var lastState = instance.state;
                var nextState = instance.state;
                var childContext = instance.getChildContext();
                nextVComponent.instance = instance;
                instance._isSVG = isSVG;
                if (!isNullOrUndef(childContext)) {
                    childContext = Object.assign({}, context, childContext);
                }
                else {
                    childContext = context;
                }
                var lastInput$2 = instance._lastInput;
                var nextInput$2 = instance._updateComponent(lastState, nextState, lastProps, nextProps, context, false);
                instance._childContext = childContext;
                if (nextInput$2 === NO_OP) {
                    nextInput$2 = lastInput$2;
                }
                else if (isInvalid(nextInput$2)) {
                    nextInput$2 = createVPlaceholder();
                }
                instance._lastInput = nextInput$2;
                instance._vComponent = nextVComponent;
                instance._lastInput = nextInput$2;
                patch(lastInput$2, nextInput$2, parentDom, lifecycle, childContext, isSVG, shallowUnmount);
                instance.componentDidUpdate(lastProps, lastState);
                nextVComponent.dom = nextInput$2.dom;
                componentToDOMNodeMap.set(instance, nextInput$2.dom);
            }
        }
        else {
            var shouldUpdate = true;
            var lastProps$1 = lastVComponent.props;
            var nextHooks = nextVComponent.hooks;
            var nextHooksDefined = !isNullOrUndef(nextHooks);
            var lastInput$3 = lastVComponent.instance;
            nextVComponent.dom = lastVComponent.dom;
            nextVComponent.instance = lastInput$3;
            if (nextHooksDefined && !isNullOrUndef(nextHooks.onComponentShouldUpdate)) {
                shouldUpdate = nextHooks.onComponentShouldUpdate(lastProps$1, nextProps);
            }
            if (shouldUpdate !== false) {
                if (nextHooksDefined && !isNullOrUndef(nextHooks.onComponentWillUpdate)) {
                    nextHooks.onComponentWillUpdate(lastProps$1, nextProps);
                }
                var nextInput$3 = nextComponent(nextProps, context);
                if (nextInput$3 === NO_OP) {
                    return false;
                }
                else if (isInvalid(nextInput$3)) {
                    nextInput$3 = createVPlaceholder();
                }
                patch(lastInput$3, nextInput$3, parentDom, lifecycle, context, isSVG, shallowUnmount);
                nextVComponent.instance = nextInput$3;
                if (nextHooksDefined && !isNullOrUndef(nextHooks.onComponentDidUpdate)) {
                    nextHooks.onComponentDidUpdate(lastProps$1, nextProps);
                }
            }
        }
    }
    return false;
}
function patchVText(lastVText, nextVText) {
    var nextText = nextVText.text;
    var dom = lastVText.dom;
    nextVText.dom = dom;
    if (lastVText.text !== nextText) {
        dom.nodeValue = nextText;
    }
}
function patchVPlaceholder(lastVPlacholder, nextVPlacholder) {
    nextVPlacholder.dom = lastVPlacholder.dom;
}
function patchVFragment(lastVFragment, nextVFragment, parentDom, lifecycle, context, isSVG, shallowUnmount) {
    var lastChildren = lastVFragment.children;
    var nextChildren = nextVFragment.children;
    var pointer = lastVFragment.pointer;
    nextVFragment.dom = lastVFragment.dom;
    nextVFragment.pointer = pointer;
    if (!lastChildren !== nextChildren) {
        var lastChildrenType = lastVFragment.childrenType;
        var nextChildrenType = nextVFragment.childrenType;
        if (lastChildrenType === nextChildrenType) {
            if (isKeyedListChildrenType(nextChildrenType)) {
                return patchKeyedChildren(lastChildren, nextChildren, parentDom, lifecycle, context, isSVG, nextVFragment, shallowUnmount);
            }
            else if (isNonKeyedListChildrenType(nextChildrenType)) {
                return patchNonKeyedChildren(lastChildren, nextChildren, parentDom, lifecycle, context, isSVG, nextVFragment, false, shallowUnmount);
            }
        }
        if (isKeyed(lastChildren, nextChildren)) {
            patchKeyedChildren(lastChildren, nextChildren, parentDom, lifecycle, context, isSVG, nextVFragment, shallowUnmount);
        }
        else {
            patchNonKeyedChildren(lastChildren, nextChildren, parentDom, lifecycle, context, isSVG, nextVFragment, true, shallowUnmount);
        }
    }
}
function patchNonKeyedChildren(lastChildren, nextChildren, dom, lifecycle, context, isSVG, parentVList, shouldNormalise, shallowUnmount) {
    var lastChildrenLength = lastChildren.length;
    var nextChildrenLength = nextChildren.length;
    var commonLength = lastChildrenLength > nextChildrenLength ? nextChildrenLength : lastChildrenLength;
    var i = 0;
    for (; i < commonLength; i++) {
        var lastChild = lastChildren[i];
        var nextChild = shouldNormalise ? normaliseChild(nextChildren, i) : nextChildren[i];
        patch(lastChild, nextChild, dom, lifecycle, context, isSVG, shallowUnmount);
    }
    if (lastChildrenLength < nextChildrenLength) {
        for (i = commonLength; i < nextChildrenLength; i++) {
            var child = normaliseChild(nextChildren, i);
            insertOrAppend(dom, mount(child, null, lifecycle, context, isSVG, shallowUnmount), parentVList && parentVList.pointer);
        }
    }
    else if (lastChildrenLength > nextChildrenLength) {
        for (i = commonLength; i < lastChildrenLength; i++) {
            unmount(lastChildren[i], dom, lifecycle, false, shallowUnmount);
        }
    }
}
function patchKeyedChildren(a, b, dom, lifecycle, context, isSVG, parentVList, shallowUnmount) {
    var aLength = a.length;
    var bLength = b.length;
    var aEnd = aLength - 1;
    var bEnd = bLength - 1;
    var aStart = 0;
    var bStart = 0;
    var i;
    var j;
    var aStartNode = a[aStart];
    var bStartNode = b[bStart];
    var aEndNode = a[aEnd];
    var bEndNode = b[bEnd];
    var aNode;
    var bNode;
    var nextNode;
    var nextPos;
    var node;
    if (aLength === 0) {
        if (bLength !== 0) {
            mountArrayChildrenWithType(b, dom, lifecycle, context, isSVG, shallowUnmount);
        }
        return;
    }
    else if (bLength === 0) {
        if (aLength !== 0) {
            removeAllChildren(dom, a, lifecycle, shallowUnmount);
        }
        return;
    }
    // Step 1
    /* eslint no-constant-condition: 0 */
    outer: while (true) {
        // Sync nodes with the same key at the beginning.
        while (aStartNode.key === bStartNode.key) {
            patch(aStartNode, bStartNode, dom, lifecycle, context, isSVG, shallowUnmount);
            aStart++;
            bStart++;
            if (aStart > aEnd || bStart > bEnd) {
                break outer;
            }
            aStartNode = a[aStart];
            bStartNode = b[bStart];
        }
        // Sync nodes with the same key at the end.
        while (aEndNode.key === bEndNode.key) {
            patch(aEndNode, bEndNode, dom, lifecycle, context, isSVG, shallowUnmount);
            aEnd--;
            bEnd--;
            if (aStart > aEnd || bStart > bEnd) {
                break outer;
            }
            aEndNode = a[aEnd];
            bEndNode = b[bEnd];
        }
        // Move and sync nodes from right to left.
        if (aEndNode.key === bStartNode.key) {
            patch(aEndNode, bStartNode, dom, lifecycle, context, isSVG, shallowUnmount);
            insertOrAppend(dom, bStartNode.dom, aStartNode.dom);
            aEnd--;
            bStart++;
            if (aStart > aEnd || bStart > bEnd) {
                break;
            }
            aEndNode = a[aEnd];
            bStartNode = b[bStart];
            // In a real-world scenarios there is a higher chance that next node after the move will be the same, so we
            // immediately jump to the start of this prefix/suffix algo.
            continue;
        }
        // Move and sync nodes from left to right.
        if (aStartNode.key === bEndNode.key) {
            patch(aStartNode, bEndNode, dom, lifecycle, context, isSVG, shallowUnmount);
            nextPos = bEnd + 1;
            nextNode = nextPos < b.length ? b[nextPos].dom : parentVList && parentVList.pointer;
            insertOrAppend(dom, bEndNode.dom, nextNode);
            aStart++;
            bEnd--;
            if (aStart > aEnd || bStart > bEnd) {
                break;
            }
            aStartNode = a[aStart];
            bEndNode = b[bEnd];
            continue;
        }
        break;
    }
    if (aStart > aEnd) {
        if (bStart <= bEnd) {
            nextPos = bEnd + 1;
            nextNode = nextPos < b.length ? b[nextPos].dom : parentVList && parentVList.pointer;
            while (bStart <= bEnd) {
                insertOrAppend(dom, mount(b[bStart++], null, lifecycle, context, isSVG, shallowUnmount), nextNode);
            }
        }
    }
    else if (bStart > bEnd) {
        while (aStart <= aEnd) {
            unmount(a[aStart++], dom, lifecycle, false, shallowUnmount);
        }
    }
    else {
        aLength = aEnd - aStart + 1;
        bLength = bEnd - bStart + 1;
        var aNullable = a;
        var sources = new Array(bLength);
        // Mark all nodes as inserted.
        for (i = 0; i < bLength; i++) {
            sources[i] = -1;
        }
        var moved = false;
        var pos = 0;
        var patched = 0;
        if ((bLength <= 4) || (aLength * bLength <= 16)) {
            for (i = aStart; i <= aEnd; i++) {
                aNode = a[i];
                if (patched < bLength) {
                    for (j = bStart; j <= bEnd; j++) {
                        bNode = b[j];
                        if (aNode.key === bNode.key) {
                            sources[j - bStart] = i;
                            if (pos > j) {
                                moved = true;
                            }
                            else {
                                pos = j;
                            }
                            patch(aNode, bNode, dom, lifecycle, context, isSVG, shallowUnmount);
                            patched++;
                            aNullable[i] = null;
                            break;
                        }
                    }
                }
            }
        }
        else {
            var keyIndex = new Map();
            for (i = bStart; i <= bEnd; i++) {
                node = b[i];
                keyIndex.set(node.key, i);
            }
            for (i = aStart; i <= aEnd; i++) {
                aNode = a[i];
                if (patched < bLength) {
                    j = keyIndex.get(aNode.key);
                    if (!isUndefined(j)) {
                        bNode = b[j];
                        sources[j - bStart] = i;
                        if (pos > j) {
                            moved = true;
                        }
                        else {
                            pos = j;
                        }
                        patch(aNode, bNode, dom, lifecycle, context, isSVG, shallowUnmount);
                        patched++;
                        aNullable[i] = null;
                    }
                }
            }
        }
        if (aLength === a.length && patched === 0) {
            removeAllChildren(dom, a, lifecycle, shallowUnmount);
            while (bStart < bLength) {
                insertOrAppend(dom, mount(b[bStart++], null, lifecycle, context, isSVG, shallowUnmount), null);
            }
        }
        else {
            i = aLength - patched;
            while (i > 0) {
                aNode = aNullable[aStart++];
                if (!isNull(aNode)) {
                    unmount(aNode, dom, lifecycle, false, shallowUnmount);
                    i--;
                }
            }
            if (moved) {
                var seq = lis_algorithm(sources);
                j = seq.length - 1;
                for (i = bLength - 1; i >= 0; i--) {
                    if (sources[i] === -1) {
                        pos = i + bStart;
                        node = b[pos];
                        nextPos = pos + 1;
                        nextNode = nextPos < b.length ? b[nextPos].dom : parentVList && parentVList.pointer;
                        insertOrAppend(dom, mount(node, dom, lifecycle, context, isSVG, shallowUnmount), nextNode);
                    }
                    else {
                        if (j < 0 || i !== seq[j]) {
                            pos = i + bStart;
                            node = b[pos];
                            nextPos = pos + 1;
                            nextNode = nextPos < b.length ? b[nextPos].dom : parentVList && parentVList.pointer;
                            insertOrAppend(dom, node.dom, nextNode);
                        }
                        else {
                            j--;
                        }
                    }
                }
            }
            else if (patched !== bLength) {
                for (i = bLength - 1; i >= 0; i--) {
                    if (sources[i] === -1) {
                        pos = i + bStart;
                        node = b[pos];
                        nextPos = pos + 1;
                        nextNode = nextPos < b.length ? b[nextPos].dom : parentVList && parentVList.pointer;
                        insertOrAppend(dom, mount(node, null, lifecycle, context, isSVG, shallowUnmount), nextNode);
                    }
                }
            }
        }
    }
}
// https://en.wikipedia.org/wiki/Longest_increasing_subsequence
function lis_algorithm(a) {
    var p = a.slice(0);
    var result = [];
    result.push(0);
    var i;
    var j;
    var u;
    var v;
    var c;
    for (i = 0; i < a.length; i++) {
        if (a[i] === -1) {
            continue;
        }
        j = result[result.length - 1];
        if (a[j] < a[i]) {
            p[i] = j;
            result.push(i);
            continue;
        }
        u = 0;
        v = result.length - 1;
        while (u < v) {
            c = ((u + v) / 2) | 0;
            if (a[result[c]] < a[i]) {
                u = c + 1;
            }
            else {
                v = c;
            }
        }
        if (a[i] < a[result[u]]) {
            if (u > 0) {
                p[i] = result[u - 1];
            }
            result[u] = i;
        }
    }
    u = result.length;
    v = result[u - 1];
    while (u-- > 0) {
        result[u] = v;
        v = p[v];
    }
    return result;
}
// returns true if a property has been applied that can't be cloned via elem.cloneNode()
function patchProp(prop, lastValue, nextValue, dom) {
    if (strictProps[prop]) {
        dom[prop] = isNullOrUndef(nextValue) ? '' : nextValue;
    }
    else if (booleanProps[prop]) {
        dom[prop] = nextValue ? true : false;
    }
    else {
        if (lastValue !== nextValue) {
            if (isNullOrUndef(nextValue)) {
                dom.removeAttribute(prop);
                return false;
            }
            if (prop === 'className') {
                dom.className = nextValue;
                return false;
            }
            else if (prop === 'style') {
                patchStyle(lastValue, nextValue, dom);
            }
            else if (prop === 'defaultChecked') {
                if (isNull(lastValue)) {
                    dom.addAttribute('checked');
                }
                return false;
            }
            else if (prop === 'defaultValue') {
                if (isNull(lastValue)) {
                    dom.setAttribute('value', nextValue);
                }
                return false;
            }
            else if (isAttrAnEvent(prop)) {
                dom[prop.toLowerCase()] = nextValue;
            }
            else if (prop === 'dangerouslySetInnerHTML') {
                var lastHtml = lastValue && lastValue.__html;
                var nextHtml = nextValue && nextValue.__html;
                if (isNullOrUndef(nextHtml)) {
                    if ("development" !== 'production') {
                        throwError('dangerouslySetInnerHTML requires an object with a __html propety containing the innerHTML content.');
                    }
                    throwError();
                }
                if (lastHtml !== nextHtml) {
                    dom.innerHTML = nextHtml;
                }
            }
            else if (prop !== 'childrenType' && prop !== 'ref' && prop !== 'key') {
                var ns = namespaces[prop];
                if (ns) {
                    dom.setAttributeNS(ns, prop, nextValue);
                }
                else {
                    dom.setAttribute(prop, nextValue);
                }
                return false;
            }
        }
    }
    return true;
}
function patchProps(lastProps, nextProps, dom, shallowUnmount) {
    lastProps = lastProps || {};
    nextProps = nextProps || {};
    var formValue;
    for (var prop in nextProps) {
        var nextValue = nextProps[prop];
        var lastValue = lastProps[prop];
        if (prop === 'value') {
            formValue = nextValue;
        }
        if (isNullOrUndef(nextValue)) {
            removeProp(prop, dom);
        }
        else {
            patchProp(prop, lastValue, nextValue, dom);
        }
    }
    for (var prop$1 in lastProps) {
        if (isNullOrUndef(nextProps[prop$1])) {
            removeProp(prop$1, dom);
        }
    }
    return formValue;
}
function patchStyle(lastAttrValue, nextAttrValue, dom) {
    if (isString(nextAttrValue)) {
        dom.style.cssText = nextAttrValue;
    }
    else if (isNullOrUndef(lastAttrValue)) {
        if (!isNullOrUndef(nextAttrValue)) {
            var styleKeys = Object.keys(nextAttrValue);
            for (var i = 0; i < styleKeys.length; i++) {
                var style = styleKeys[i];
                var value = nextAttrValue[style];
                if (isNumber(value) && !isUnitlessNumber[style]) {
                    dom.style[style] = value + 'px';
                }
                else {
                    dom.style[style] = value;
                }
            }
        }
    }
    else if (isNullOrUndef(nextAttrValue)) {
        dom.removeAttribute('style');
    }
    else {
        var styleKeys$1 = Object.keys(nextAttrValue);
        for (var i$1 = 0; i$1 < styleKeys$1.length; i$1++) {
            var style$1 = styleKeys$1[i$1];
            var value$1 = nextAttrValue[style$1];
            if (isNumber(value$1) && !isUnitlessNumber[style$1]) {
                dom.style[style$1] = value$1 + 'px';
            }
            else {
                dom.style[style$1] = value$1;
            }
        }
        var lastStyleKeys = Object.keys(lastAttrValue);
        for (var i$2 = 0; i$2 < lastStyleKeys.length; i$2++) {
            var style$2 = lastStyleKeys[i$2];
            if (isNullOrUndef(nextAttrValue[style$2])) {
                dom.style[style$2] = '';
            }
        }
    }
}
function removeProp(prop, dom) {
    if (prop === 'className') {
        dom.removeAttribute('class');
    }
    else if (prop === 'value') {
        dom.value = '';
    }
    else {
        dom.removeAttribute(prop);
    }
}

function convertVOptElementToVElement(optVElement) {
    var bp = optVElement.bp;
    var staticElement = bp.staticVElement;
    var vElement = createVElement(staticElement.tag, null, null, optVElement.key, null, null);
    var bp0 = bp.v0;
    var staticChildren = staticElement.children;
    var staticProps = staticElement.props;
    if (!isNull(staticChildren)) {
        vElement.children = staticChildren;
    }
    if (!isNull(staticProps)) {
        vElement.props = staticProps;
    }
    if (!isNull(bp0)) {
        attachOptVElementValue(vElement, optVElement, bp0, optVElement.v0, bp.d0);
        var bp1 = bp.v1;
        if (!isNull(bp1)) {
            attachOptVElementValue(vElement, optVElement, bp1, optVElement.v1, bp.d1);
            var bp2 = bp.v2;
            if (!isNull(bp2)) {
                attachOptVElementValue(vElement, optVElement, bp2, optVElement.v2, bp.d2);
                var bp3 = bp.v3;
                if (!isNull(bp3)) {
                    var v3 = optVElement.v3;
                    var d3 = bp.d3;
                    var bp3$1 = bp.v3;
                    for (var i = 0; i < bp3$1.length; i++) {
                        attachOptVElementValue(vElement, optVElement, bp3$1[i], v3[i], d3[i]);
                    }
                }
            }
        }
    }
    return vElement;
}
function attachOptVElementValue(vElement, vOptElement, valueType, value, descriptor) {
    switch (valueType) {
        case ValueTypes.CHILDREN:
            vElement.childrenType = descriptor;
            if (isNullOrUndef(vElement.children)) {
                vElement.children = value;
            }
            else {
                debugger;
            }
            break;
        case ValueTypes.PROP_CLASS_NAME:
            if (!vElement.props) {
                vElement.props = { className: value };
            }
            else {
                debugger;
            }
            break;
        case ValueTypes.PROP_DATA:
            if (!vElement.props) {
                vElement.props = {};
            }
            vElement.props['data-' + descriptor] = value;
            break;
        case ValueTypes.PROP_STYLE:
            if (!vElement.props) {
                vElement.props = { style: value };
            }
            else {
                debugger;
            }
            break;
        case ValueTypes.PROP_VALUE:
            if (!vElement.props) {
                vElement.props = { value: value };
            }
            else {
                debugger;
            }
            break;
        case ValueTypes.PROP:
            if (!vElement.props) {
                vElement.props = {};
            }
            vElement.props[descriptor] = value;
            break;
        case ValueTypes.PROP_REF:
            vElement.ref = value;
            break;
        case ValueTypes.PROP_SPREAD:
            if (!vElement.props) {
                vElement.props = value;
            }
            else {
                debugger;
            }
            break;
    }
}
function cloneVNode(vNodeToClone, props) {
    var _children = [], len = arguments.length - 2;
    while ( len-- > 0 ) _children[ len ] = arguments[ len + 2 ];

    var children = _children;
    if (_children.length > 0 && !isNull(_children[0])) {
        if (!props) {
            props = {};
        }
        if (_children.length === 1) {
            children = _children[0];
        }
        if (isUndefined(props.children)) {
            props.children = children;
        }
        else {
            if (isArray(children)) {
                if (isArray(props.children)) {
                    props.children = props.children.concat(children);
                }
                else {
                    props.children = [props.children].concat(children);
                }
            }
            else {
                if (isArray(props.children)) {
                    props.children.push(children);
                }
                else {
                    props.children = [props.children];
                    props.children.push(children);
                }
            }
        }
    }
    else {
        children = null;
    }
    var newVNode;
    if (isArray(vNodeToClone)) {
        newVNode = vNodeToClone.map(function (vNode) { return cloneVNode(vNode); });
    }
    else if (isNullOrUndef(props) && isNullOrUndef(children)) {
        newVNode = Object.assign({}, vNodeToClone);
    }
    else {
        if (isVComponent(vNodeToClone)) {
            newVNode = createVComponent(vNodeToClone.component, Object.assign({}, vNodeToClone.props, props), vNodeToClone.key, vNodeToClone.hooks, vNodeToClone.ref);
        }
        else if (isVElement(vNodeToClone)) {
            newVNode = createVElement(vNodeToClone.tag, Object.assign({}, vNodeToClone.props, props), (props && props.children) || children || vNodeToClone.children, vNodeToClone.key, vNodeToClone.ref, ChildrenTypes.UNKNOWN);
        }
        else if (isOptVElement(vNodeToClone)) {
            newVNode = cloneVNode(convertVOptElementToVElement(vNodeToClone), props, children);
        }
    }
    newVNode.dom = null;
    return newVNode;
}

function copyPropsTo(copyFrom, copyTo) {
    for (var prop in copyFrom) {
        if (isUndefined(copyTo[prop])) {
            copyTo[prop] = copyFrom[prop];
        }
    }
}
function createStatefulComponentInstance(Component, props, context, isSVG) {
    var instance = new Component(props, context);
    instance.context = context;
    instance._patch = patch;
    instance._componentToDOMNodeMap = componentToDOMNodeMap;
    var childContext = instance.getChildContext();
    if (!isNullOrUndef(childContext)) {
        instance._childContext = Object.assign({}, context, childContext);
    }
    else {
        instance._childContext = context;
    }
    instance._unmounted = false;
    instance._pendingSetState = true;
    instance._isSVG = isSVG;
    instance.componentWillMount();
    var input = instance.render();
    if (isInvalid(input)) {
        input = createVPlaceholder();
    }
    instance._pendingSetState = false;
    instance._lastInput = input;
    return instance;
}
function createStatelessComponentInput(component, props, context) {
    var input = component(props, context);
    if (isInvalid(input)) {
        input = createVPlaceholder();
    }
    return input;
}
function setTextContent(dom, text) {
    if (text !== '') {
        dom.textContent = text;
    }
    else {
        dom.appendChild(document.createTextNode(''));
    }
}
function updateTextContent(dom, text) {
    dom.firstChild.nodeValue = text;
}
function appendChild(parentDom, dom) {
    parentDom.appendChild(dom);
}
function insertOrAppend(parentDom, newNode, nextNode) {
    if (isNullOrUndef(nextNode)) {
        appendChild(parentDom, newNode);
    }
    else {
        parentDom.insertBefore(newNode, nextNode);
    }
}
function replaceVListWithNode(parentDom, vList, dom, lifecycle, shallowUnmount) {
    var pointer = vList.pointer;
    unmountVFragment(vList, parentDom, false, lifecycle, shallowUnmount);
    replaceChild(parentDom, dom, pointer);
}
function getPropFromOptElement(optVElement, valueType) {
    var bp = optVElement.bp;
    // TODO check "prop" and "spread"
    if (!isNull(bp.v0)) {
        if (bp.v0 === valueType) {
            return optVElement.v0;
        }
        if (!isNull(bp.v1)) {
            if (bp.v1 === valueType) {
                return optVElement.v1;
            }
            if (!isNull(bp.v2)) {
                if (bp.v2 === valueType) {
                    return optVElement.v2;
                }
            }
        }
    }
}
function documentCreateElement(tag, isSVG) {
    var dom;
    if (isSVG === true) {
        dom = document.createElementNS(svgNS, tag);
    }
    else {
        dom = document.createElement(tag);
    }
    return dom;
}
function replaceWithNewNode(lastNode, nextNode, parentDom, lifecycle, context, isSVG, shallowUnmount) {
    var lastInstance = null;
    var instanceLastNode = lastNode._lastInput;
    if (!isNullOrUndef(instanceLastNode)) {
        lastInstance = lastNode;
        lastNode = instanceLastNode;
    }
    unmount(lastNode, null, lifecycle, true, shallowUnmount);
    var dom = mount(nextNode, null, lifecycle, context, isSVG, shallowUnmount);
    nextNode.dom = dom;
    replaceChild(parentDom, dom, lastNode.dom);
    if (lastInstance !== null) {
        lastInstance._lasInput = nextNode;
    }
}
function replaceChild(parentDom, nextDom, lastDom) {
    parentDom.replaceChild(nextDom, lastDom);
}
function normalise(object) {
    if (isStringOrNumber(object)) {
        return createVText(object);
    }
    else if (isInvalid(object)) {
        return createVPlaceholder();
    }
    else if (isArray(object)) {
        return createVFragment(object, null);
    }
    else if (isVNode(object) && object.dom) {
        return cloneVNode(object);
    }
    return object;
}
function normaliseChild(children, i) {
    var child = children[i];
    children[i] = normalise(child);
    return children[i];
}
function removeChild(parentDom, dom) {
    parentDom.removeChild(dom);
}
// TODO: for node we need to check if document is valid
function getActiveNode() {
    return document.activeElement;
}
function removeAllChildren(dom, children, lifecycle, shallowUnmount) {
    dom.textContent = '';
    for (var i = 0; i < children.length; i++) {
        var child = children[i];
        if (!isInvalid(child)) {
            unmount(child, null, lifecycle, true, shallowUnmount);
        }
    }
}
function resetActiveNode(activeNode) {
    if (activeNode !== null && activeNode !== document.body && document.activeElement !== activeNode) {
        activeNode.focus(); // TODO: verify are we doing new focus event, if user has focus listener this might trigger it
    }
}
function isKeyed(lastChildren, nextChildren) {
    if (lastChildren.complex) {
        return false;
    }
    return nextChildren.length && !isNullOrUndef(nextChildren[0]) && !isNullOrUndef(nextChildren[0].key)
        && lastChildren.length && !isNullOrUndef(lastChildren[0]) && !isNullOrUndef(lastChildren[0].key);
}
function formSelectValueFindOptions(dom, value, isMap) {
    var child = dom.firstChild;
    while (child) {
        var tagName = child.tagName;
        if (tagName === 'OPTION') {
            child.selected = !!((!isMap && child.value === value) || (isMap && value.get(child.value)));
        }
        else if (tagName === 'OPTGROUP') {
            formSelectValueFindOptions(child, value, isMap);
        }
        child = child.nextSibling;
    }
}
function formSelectValue(dom, value) {
    var isMap = false;
    if (!isNullOrUndef(value)) {
        if (isArray(value)) {
            // Map vs Object v using reduce here for perf?
            value = value.reduce(function (o, v) { return o.set(v, true); }, new Map());
            isMap = true;
        }
        else {
            // convert to string
            value = value + '';
        }
        formSelectValueFindOptions(dom, value, isMap);
    }
}
function resetFormInputProperties(dom) {
    if (dom.checked) {
        dom.checked = false;
    }
    if (dom.disabled) {
        dom.disabled = false;
    }
}

function mountStaticChildren(children, dom, isSVG) {
    if (isArray(children)) {
        for (var i = 0; i < children.length; i++) {
            var child = children[i];
            mountStaticChildren(child, dom, isSVG);
        }
    }
    else if (isStringOrNumber(children)) {
        dom.appendChild(document.createTextNode(children));
    }
    else if (!isInvalid(children)) {
        mountStaticNode(children, dom, isSVG);
    }
}
function mountStaticNode(node, parentDom, isSVG) {
    var tag = node.tag;
    if (tag === 'svg') {
        isSVG = true;
    }
    var dom = documentCreateElement(tag, isSVG);
    var children = node.children;
    if (!isNull(children)) {
        mountStaticChildren(children, dom, isSVG);
    }
    var props = node.props;
    if (!isNull(props)) {
        for (var prop in props) {
            patchProp(prop, null, props[prop], dom);
        }
    }
    if (parentDom) {
        parentDom.appendChild(dom);
    }
    return dom;
}
function createStaticVElementClone(bp, isSVG) {
    if (!isBrowser) {
        return null;
    }
    var staticNode = bp.staticVElement;
    var dom = mountStaticNode(staticNode, null, isSVG);
    if (isSVG) {
        bp.svgClone = dom;
    }
    else {
        bp.clone = dom;
    }
    return dom.cloneNode(true);
}

function mount(input, parentDom, lifecycle, context, isSVG, shallowUnmount) {
    if (isOptVElement(input)) {
        return mountOptVElement(input, parentDom, lifecycle, context, isSVG, shallowUnmount);
    }
    else if (isVComponent(input)) {
        return mountVComponent(input, parentDom, lifecycle, context, isSVG, shallowUnmount);
    }
    else if (isVElement(input)) {
        return mountVElement(input, parentDom, lifecycle, context, isSVG, shallowUnmount);
    }
    else if (isVText(input)) {
        return mountVText(input, parentDom);
    }
    else if (isVFragment(input)) {
        return mountVFragment(input, parentDom, lifecycle, context, isSVG, shallowUnmount);
    }
    else if (isVPlaceholder(input)) {
        return mountVPlaceholder(input, parentDom);
    }
    else {
        if ("development" !== 'production') {
            throwError('bad input argument called on mount(). Input argument may need normalising.');
        }
        throwError();
    }
}
function mountVPlaceholder(vPlaceholder, parentDom) {
    var dom = document.createTextNode('');
    vPlaceholder.dom = dom;
    if (parentDom) {
        appendChild(parentDom, dom);
    }
    return dom;
}
function mountVElement(vElement, parentDom, lifecycle, context, isSVG, shallowUnmount) {
    var tag = vElement.tag;
    if (!isString(tag)) {
        if ("development" !== 'production') {
            throwError('expects VElement to have a string as the tag name');
        }
        throwError();
    }
    if (tag === 'svg') {
        isSVG = true;
    }
    var dom = documentCreateElement(tag, isSVG);
    var children = vElement.children;
    var props = vElement.props;
    var ref = vElement.ref;
    var hasProps = !isNullOrUndef(props);
    var formValue;
    vElement.dom = dom;
    if (!isNullOrUndef(ref)) {
        mountRef(dom, ref, lifecycle);
    }
    if (hasProps) {
        formValue = mountProps(vElement, props, dom, lifecycle, context, isSVG, false, shallowUnmount);
    }
    if (!isNullOrUndef(children)) {
        mountChildren(vElement.childrenType, children, dom, lifecycle, context, isSVG, shallowUnmount);
    }
    if (tag === 'select' && formValue) {
        formSelectValue(dom, formValue);
    }
    if (!isNull(parentDom)) {
        appendChild(parentDom, dom);
    }
    return dom;
}
function mountVFragment(vFragment, parentDom, lifecycle, context, isSVG, shallowUnmount) {
    var children = vFragment.children;
    var pointer = document.createTextNode('');
    var dom = document.createDocumentFragment();
    var childrenType = vFragment.childrenType;
    if (isKeyedListChildrenType(childrenType) || isNonKeyedListChildrenType(childrenType)) {
        mountArrayChildrenWithType(children, dom, lifecycle, context, isSVG, shallowUnmount);
    }
    else if (isUnknownChildrenType(childrenType)) {
        mountArrayChildrenWithoutType(children, dom, lifecycle, context, isSVG, shallowUnmount);
    }
    vFragment.pointer = pointer;
    vFragment.dom = dom;
    appendChild(dom, pointer);
    if (parentDom) {
        appendChild(parentDom, dom);
    }
    return dom;
}
function mountVText(vText, parentDom) {
    var dom = document.createTextNode(vText.text);
    vText.dom = dom;
    if (!isNull(parentDom)) {
        appendChild(parentDom, dom);
    }
    return dom;
}
function mountOptVElement(optVElement, parentDom, lifecycle, context, isSVG, shallowUnmount) {
    var bp = optVElement.bp;
    var dom = null;
    if (recyclingEnabled) {
        dom = recycleOptVElement(optVElement, lifecycle, context, isSVG, shallowUnmount);
    }
    var tag = bp.staticVElement.tag;
    if (isNull(dom)) {
        if (isSVG || tag === 'svg') {
            isSVG = true;
            dom = (bp.svgClone && bp.svgClone.cloneNode(true)) || createStaticVElementClone(bp, isSVG);
        }
        else {
            dom = (bp.clone && bp.clone.cloneNode(true)) || createStaticVElementClone(bp, isSVG);
        }
        optVElement.dom = dom;
        var bp0 = bp.v0;
        if (!isNull(bp0)) {
            mountOptVElementValue(optVElement, bp0, optVElement.v0, bp.d0, dom, lifecycle, context, isSVG, shallowUnmount);
            var bp1 = bp.v1;
            if (!isNull(bp1)) {
                mountOptVElementValue(optVElement, bp1, optVElement.v1, bp.d1, dom, lifecycle, context, isSVG, shallowUnmount);
                var bp2 = bp.v2;
                if (!isNull(bp2)) {
                    mountOptVElementValue(optVElement, bp2, optVElement.v2, bp.d2, dom, lifecycle, context, isSVG, shallowUnmount);
                    var bp3 = bp.v3;
                    if (!isNull(bp3)) {
                        var v3 = optVElement.v3;
                        var d3 = bp.d3;
                        var bp3$1 = bp.v3;
                        for (var i = 0; i < bp3$1.length; i++) {
                            mountOptVElementValue(optVElement, bp3$1[i], v3[i], d3[i], dom, lifecycle, context, isSVG, shallowUnmount);
                        }
                    }
                }
            }
        }
        if (tag === 'select') {
            formSelectValue(dom, getPropFromOptElement(optVElement, ValueTypes.PROP_VALUE));
        }
    }
    if (!isNull(parentDom)) {
        parentDom.appendChild(dom);
    }
    return dom;
}
function mountOptVElementValue(optVElement, valueType, value, descriptor, dom, lifecycle, context, isSVG, shallowUnmount) {
    switch (valueType) {
        case ValueTypes.CHILDREN:
            mountChildren(descriptor, value, dom, lifecycle, context, isSVG, shallowUnmount);
            break;
        case ValueTypes.PROP_CLASS_NAME:
            if (!isNullOrUndef(value)) {
                if (isSVG) {
                    dom.setAttribute('class', value);
                }
                else {
                    dom.className = value;
                }
            }
            break;
        case ValueTypes.PROP_DATA:
            dom.dataset[descriptor] = value;
            break;
        case ValueTypes.PROP_STYLE:
            patchStyle(null, value, dom);
            break;
        case ValueTypes.PROP_VALUE:
            dom.value = isNullOrUndef(value) ? '' : value;
            break;
        case ValueTypes.PROP:
            patchProp(descriptor, null, value, dom);
            break;
        case ValueTypes.PROP_REF:
            mountRef(dom, value, lifecycle);
            break;
        case ValueTypes.PROP_SPREAD:
            mountProps(optVElement, value, dom, lifecycle, context, isSVG, true, shallowUnmount);
            break;
    }
}
function mountChildren(childrenType, children, dom, lifecycle, context, isSVG, shallowUnmount) {
    if (isTextChildrenType(childrenType)) {
        setTextContent(dom, children);
    }
    else if (isNodeChildrenType(childrenType)) {
        mount(children, dom, lifecycle, context, isSVG, shallowUnmount);
    }
    else if (isKeyedListChildrenType(childrenType) || isNonKeyedListChildrenType(childrenType)) {
        mountArrayChildrenWithType(children, dom, lifecycle, context, isSVG, shallowUnmount);
    }
    else if (isUnknownChildrenType(childrenType)) {
        mountChildrenWithUnknownType(children, dom, lifecycle, context, isSVG, shallowUnmount);
    }
    else {
        if ("development" !== 'production') {
            throwError('bad childrenType value specified when attempting to mountChildren.');
        }
        throwError();
    }
}
function mountArrayChildrenWithType(children, dom, lifecycle, context, isSVG, shallowUnmount) {
    for (var i = 0; i < children.length; i++) {
        mount(children[i], dom, lifecycle, context, isSVG, shallowUnmount);
    }
}
function mountChildrenWithUnknownType(children, dom, lifecycle, context, isSVG, shallowUnmount) {
    if (isArray(children)) {
        mountArrayChildrenWithoutType(children, dom, lifecycle, context, isSVG, shallowUnmount);
    }
    else if (isStringOrNumber(children)) {
        setTextContent(dom, children);
    }
    else if (!isInvalid(children)) {
        mount(children, dom, lifecycle, context, isSVG, shallowUnmount);
    }
}
function mountArrayChildrenWithoutType(children, dom, lifecycle, context, isSVG, shallowUnmount) {
    children.complex = false;
    for (var i = 0; i < children.length; i++) {
        var child = normaliseChild(children, i);
        if (isVText(child)) {
            mountVText(child, dom);
            children.complex = true;
        }
        else if (isVPlaceholder(child)) {
            mountVPlaceholder(child, dom);
            children.complex = true;
        }
        else if (isVFragment(child)) {
            mountVFragment(child, dom, lifecycle, context, isSVG, shallowUnmount);
            children.complex = true;
        }
        else {
            mount(child, dom, lifecycle, context, isSVG, shallowUnmount);
        }
    }
}
function mountVComponent(vComponent, parentDom, lifecycle, context, isSVG, shallowUnmount) {
    if (recyclingEnabled) {
        var dom$1 = recycleVComponent(vComponent, lifecycle, context, isSVG, shallowUnmount);
        if (!isNull(dom$1)) {
            if (!isNull(parentDom)) {
                appendChild(parentDom, dom$1);
            }
            return dom$1;
        }
    }
    var component = vComponent.component;
    var props = vComponent.props || EMPTY_OBJ;
    var hooks = vComponent.hooks;
    var ref = vComponent.ref;
    var dom;
    if (isStatefulComponent(vComponent)) {
        var defaultProps = component.defaultProps;
        if (!isUndefined(defaultProps)) {
            copyPropsTo(defaultProps, props);
            vComponent.props = props;
        }
        if (hooks) {
            if ("development" !== 'production') {
                throwError('"hooks" are not supported on stateful components.');
            }
            throwError();
        }
        var instance = createStatefulComponentInstance(component, props, context, isSVG);
        var input = instance._lastInput;
        instance._vComponent = vComponent;
        vComponent.dom = dom = mount(input, null, lifecycle, instance._childContext, false, shallowUnmount);
        if (!isNull(parentDom)) {
            appendChild(parentDom, dom);
        }
        mountStatefulComponentCallbacks(ref, instance, lifecycle);
        componentToDOMNodeMap.set(instance, dom);
        vComponent.instance = instance;
    }
    else {
        if (ref) {
            if ("development" !== 'production') {
                throwError('"refs" are not supported on stateless components.');
            }
            throwError();
        }
        var input$1 = createStatelessComponentInput(component, props, context);
        vComponent.dom = dom = mount(input$1, null, lifecycle, context, isSVG, shallowUnmount);
        vComponent.instance = input$1;
        mountStatelessComponentCallbacks(hooks, dom, lifecycle);
        if (!isNull(parentDom)) {
            appendChild(parentDom, dom);
        }
    }
    return dom;
}
function mountStatefulComponentCallbacks(ref, instance, lifecycle) {
    if (ref) {
        if (isFunction(ref)) {
            lifecycle.addListener(function () { return ref(instance); });
        }
        else {
            if ("development" !== 'production') {
                throwError('string "refs" are not supported in Inferno 0.8+. Use callback "refs" instead.');
            }
            throwError();
        }
    }
    if (!isNull(instance.componentDidMount)) {
        lifecycle.addListener(function () {
            instance.componentDidMount();
        });
    }
}
function mountStatelessComponentCallbacks(hooks, dom, lifecycle) {
    if (!isNullOrUndef(hooks)) {
        if (!isNullOrUndef(hooks.onComponentWillMount)) {
            hooks.onComponentWillMount();
        }
        if (!isNullOrUndef(hooks.onComponentDidMount)) {
            lifecycle.addListener(function () { return hooks.onComponentDidMount(dom); });
        }
    }
}
function mountProps(vNode, props, dom, lifecycle, context, isSVG, isSpread, shallowUnmount) {
    var formValue;
    for (var prop in props) {
        var value = props[prop];
        if (prop === 'value') {
            formValue = value;
        }
        if (prop === 'key') {
            vNode.key = value;
        }
        else if (prop === 'ref') {
            mountRef(dom, value, lifecycle);
        }
        else if (prop === 'children') {
            if (isSpread) {
                mountChildrenWithUnknownType(value, dom, lifecycle, context, isSVG, shallowUnmount);
            }
            else if (isVElement(vNode)) {
                vNode.children = value;
            }
        }
        else {
            patchProp(prop, null, value, dom);
        }
    }
    return formValue;
}
function mountRef(dom, value, lifecycle) {
    if (isFunction(value)) {
        lifecycle.addListener(function () { return value(dom); });
    }
    else {
        if (isInvalid(value)) {
            return;
        }
        if ("development" !== 'production') {
            throwError('string "refs" are not supported in Inferno 0.8+. Use callback "refs" instead.');
        }
        throwError();
    }
}

function normaliseChildNodes(dom) {
    var rawChildNodes = dom.childNodes;
    var length = rawChildNodes.length;
    var i = 0;
    while (i < length) {
        var rawChild = rawChildNodes[i];
        if (rawChild.nodeType === 8) {
            if (rawChild.data === '!') {
                var placeholder = document.createTextNode('');
                dom.replaceChild(placeholder, rawChild);
                i++;
            }
            else {
                dom.removeChild(rawChild);
                length--;
            }
        }
        else {
            i++;
        }
    }
}
function hydrateVComponent(vComponent, dom, lifecycle, context) {
    var component = vComponent.component;
    var props = vComponent.props;
    var hooks = vComponent.hooks;
    var ref = vComponent.ref;
    vComponent.dom = dom;
    if (isStatefulComponent(vComponent)) {
        var isSVG = dom.namespaceURI === svgNS;
        var instance = createStatefulComponentInstance(component, props, context, isSVG);
        var input = instance._lastInput;
        instance._vComponent = vComponent;
        hydrate(input, dom, lifecycle, context);
        mountStatefulComponentCallbacks(ref, instance, lifecycle);
        componentToDOMNodeMap.set(instance, dom);
        vComponent.instance = instance;
    }
    else {
        var input$1 = createStatelessComponentInput(component, props, context);
        hydrate(input$1, dom, lifecycle, context);
        vComponent.instance = input$1;
        vComponent.dom = input$1.dom;
        mountStatelessComponentCallbacks(hooks, dom, lifecycle);
    }
}
function hydrateVElement(vElement, dom, lifecycle, context) {
    var tag = vElement.tag;
    if (!isString(tag)) {
        if ("development" !== 'production') {
            throwError('expects VElement to have a string as the tag name');
        }
        throwError();
    }
    var children = vElement.children;
    vElement.dom = dom;
    if (children) {
        hydrateChildren(vElement.childrenType, children, dom, lifecycle, context);
    }
}
function hydrateArrayChildrenWithType(children, dom, lifecycle, context) {
    var domNodes = Array.prototype.slice.call(dom.childNodes);
    for (var i = 0; i < children.length; i++) {
        hydrate(children[i], domNodes[i], lifecycle, context);
    }
}
function hydrateChildrenWithUnknownType(children, dom, lifecycle, context) {
    var domNodes = Array.prototype.slice.call(dom.childNodes);
    if (isArray(children)) {
        for (var i = 0; i < children.length; i++) {
            var child = normaliseChild(children, i);
            if (isObject(child)) {
                hydrate(child, domNodes[i], lifecycle, context);
            }
        }
    }
    else if (isObject(children)) {
        hydrate(children, dom.firstChild, lifecycle, context);
    }
}
function hydrateChildren(childrenType, children, dom, lifecycle, context) {
    if (isNodeChildrenType(childrenType)) {
        hydrate(children, dom.firstChild, lifecycle, context);
    }
    else if (isKeyedListChildrenType(childrenType) || isNonKeyedListChildrenType(childrenType)) {
        hydrateArrayChildrenWithType(children, dom, lifecycle, context);
    }
    else if (isUnknownChildrenType(childrenType)) {
        hydrateChildrenWithUnknownType(children, dom, lifecycle, context);
    }
    else if (!isTextChildrenType(childrenType)) {
        if ("development" !== 'production') {
            throwError('Bad childrenType value specified when attempting to hydrateChildren.');
        }
        throwError();
    }
}
function hydrateStaticVElement(node, dom) {
    var children = node.children;
    if (!isNull(children)) {
        if (!isStringOrNumber(children) && !isInvalid(children)) {
            var childNode = dom.firstChild;
            if (isArray(children)) {
                for (var i = 0; i < children.length; i++) {
                    var child = children[i];
                    if (!isStringOrNumber(child) && !isInvalid(child)) {
                        normaliseChildNodes(childNode);
                        hydrateStaticVElement(child, normaliseChildNodes(childNode));
                    }
                    childNode = childNode.nextSibling;
                }
            }
            else {
                normaliseChildNodes(childNode);
                hydrateStaticVElement(children, childNode);
            }
        }
    }
}
function hydrateOptVElement(optVElement, dom, lifecycle, context) {
    var bp = optVElement.bp;
    var bp0 = bp.v0;
    var staticVElement = bp.staticVElement;
    hydrateStaticVElement(staticVElement, dom);
    optVElement.dom = dom;
    if (!isNull(bp0)) {
        hydrateOptVElementValue(optVElement, bp0, optVElement.v0, bp.d0, dom, lifecycle, context);
        var bp1 = bp.v1;
        if (!isNull(bp1)) {
            hydrateOptVElementValue(optVElement, bp1, optVElement.v1, bp.d1, dom, lifecycle, context);
            var bp2 = bp.v2;
            if (!isNull(bp2)) {
                hydrateOptVElementValue(optVElement, bp2, optVElement.v2, bp.d2, dom, lifecycle, context);
                var bp3 = bp.v3;
                if (!isNull(bp3)) {
                    var v3 = optVElement.v3;
                    var d3 = bp.d3;
                    var bp3$1 = bp.v3;
                    for (var i = 0; i < bp3$1.length; i++) {
                        hydrateOptVElementValue(optVElement, bp3$1[i], v3[i], d3[i], dom, lifecycle, context);
                    }
                }
            }
        }
    }
}
function hydrateVText(vText, dom) {
    vText.dom = dom;
}
function hydrateVFragment(vFragment, currentDom, lifecycle, context) {
    var children = vFragment.children;
    var parentDom = currentDom.parentNode;
    var pointer = vFragment.pointer = document.createTextNode('');
    for (var i = 0; i < children.length; i++) {
        var child = normaliseChild(children, i);
        var childDom = currentDom;
        if (isObject(child)) {
            hydrate(child, childDom, lifecycle, context);
        }
        currentDom = currentDom.nextSibling;
    }
    parentDom.insertBefore(pointer, currentDom);
}
function hydrateOptVElementValue(optVElement, valueType, value, descriptor, dom, lifecycle, context) {
    switch (valueType) {
        case ValueTypes.CHILDREN:
            hydrateChildren(descriptor, value, dom, lifecycle, context);
            break;
        case ValueTypes.PROP_SPREAD:
            debugger;
            break;
    }
}
function hydrate(input, dom, lifecycle, context) {
    normaliseChildNodes(dom);
    if (isOptVElement(input)) {
        hydrateOptVElement(input, dom, lifecycle, context);
    }
    else if (isVComponent(input)) {
        hydrateVComponent(input, dom, lifecycle, context);
    }
    else if (isVElement(input)) {
        hydrateVElement(input, dom, lifecycle, context);
    }
    else if (isVFragment(input)) {
        hydrateVFragment(input, dom, lifecycle, context);
    }
    else if (isVText(input)) {
        hydrateVText(input, dom);
    }
    else if (isVPlaceholder(input)) {
        debugger;
    }
    else {
        if ("development" !== 'production') {
            throwError('bad input argument called on hydrate(). Input argument may need normalising.');
        }
        throwError();
    }
}
function hydrateRoot(input, parentDom, lifecycle) {
    if (parentDom && parentDom.nodeType === 1) {
        var rootNode = parentDom.querySelector('[data-infernoroot]');
        if (rootNode && rootNode.parentNode === parentDom) {
            rootNode.removeAttribute('data-infernoroot');
            hydrate(input, rootNode, lifecycle, {});
            return true;
        }
    }
    return false;
}

var roots = new Map();
var componentToDOMNodeMap = new Map();
function findDOMNode(domNode) {
    return componentToDOMNodeMap.get(domNode) || null;
}
var documetBody = isBrowser ? document.body : null;
function render(input, parentDom) {
    var root = roots.get(parentDom);
    var lifecycle = new Lifecycle();
    if (documetBody === parentDom) {
        if ("development" !== 'production') {
            throwError('you cannot render() to the "document.body". Use an empty element as a container instead.');
        }
        throwError();
    }
    if (input === NO_OP) {
        return;
    }
    if (isUndefined(root)) {
        if (!isInvalid(input)) {
            if (input.dom) {
                input = cloneVNode(input);
            }
            if (!hydrateRoot(input, parentDom, lifecycle)) {
                mountChildrenWithUnknownType(input, parentDom, lifecycle, {}, false, false);
            }
            lifecycle.trigger();
            roots.set(parentDom, { input: input });
        }
    }
    else {
        var activeNode = getActiveNode();
        if (isNullOrUndef(input)) {
            unmount(root.input, parentDom, lifecycle, false, false);
            roots.delete(parentDom);
        }
        else {
            if (input.dom) {
                input = cloneVNode(input);
            }
            patchChildrenWithUnknownType(root.input, input, parentDom, lifecycle, {}, false, false);
        }
        lifecycle.trigger();
        root.input = input;
        resetActiveNode(activeNode);
    }
}
function createRenderer() {
    var parentDom;
    return function renderer(lastInput, nextInput) {
        if (!parentDom) {
            parentDom = lastInput;
        }
        render(nextInput, parentDom);
    };
}

var index = {
	render: render,
	findDOMNode: findDOMNode,
	createRenderer: createRenderer
};

return index;

})));
},{}],3:[function(require,module,exports){
/*!
 * inferno v1.0.0-alpha10
 * (c) 2016 Dominic Gannaway
 * Released under the MIT License.
 */
(function (global, factory) {
	typeof exports === 'object' && typeof module !== 'undefined' ? module.exports = factory() :
	typeof define === 'function' && define.amd ? define(factory) :
	(global.Inferno = factory());
}(this, (function () { 'use strict';

var NO_OP = '$NO_OP';
function isArray(obj) {
    return obj instanceof Array;
}
function isNullOrUndef(obj) {
    return isUndefined(obj) || isNull(obj);
}
function isNull(obj) {
    return obj === null;
}
function isUndefined(obj) {
    return obj === undefined;
}
function warning(condition, message) {
    if (!condition) {
        console.error(message);
    }
}

var ValueTypes = {
    CHILDREN: 1,
    PROP_CLASS_NAME: 2,
    PROP_STYLE: 3,
    PROP_DATA: 4,
    PROP_REF: 5,
    PROP_SPREAD: 6,
    PROP_VALUE: 7,
    PROP: 8
};
var ChildrenTypes = {
    NON_KEYED: 1,
    KEYED: 2,
    NODE: 3,
    TEXT: 4,
    UNKNOWN: 5
};
var NodeTypes = {
    ELEMENT: 1,
    OPT_ELEMENT: 2,
    TEXT: 3,
    FRAGMENT: 4,
    OPT_BLUEPRINT: 5,
    COMPONENT: 6,
    PLACEHOLDER: 7
};

function createOptVElement(bp, key, v0, v1, v2, v3) {
    return {
        bp: bp,
        dom: null,
        key: key,
        type: NodeTypes.OPT_ELEMENT,
        v0: v0,
        v1: v1,
        v2: v2,
        v3: v3
    };
}
function createOptBlueprint(staticVElement, v0, d0, v1, d1, v2, d2, v3, d3, renderer) {
    var bp = {
        clone: null,
        svgClone: null,
        d0: d0,
        d1: d1,
        d2: d2,
        d3: d3,
        pools: {
            nonKeyed: [],
            keyed: new Map()
        },
        staticVElement: staticVElement,
        type: NodeTypes.OPT_BLUEPRINT,
        v0: v0,
        v1: v1,
        v2: v2,
        v3: v3
    };
    if (renderer) {
        renderer.createStaticVElementClone(bp, false);
    }
    return bp;
}
function createVComponent(component, props, key, hooks, ref) {
    return {
        component: component,
        dom: null,
        hooks: hooks || null,
        instance: null,
        key: key,
        props: props,
        ref: ref || null,
        type: NodeTypes.COMPONENT
    };
}
function createVText(text) {
    return {
        dom: null,
        text: text,
        type: NodeTypes.TEXT
    };
}
function createVElement(tag, props, children, key, ref, childrenType) {
    return {
        children: children,
        childrenType: childrenType || ChildrenTypes.UNKNOWN,
        dom: null,
        key: key,
        props: props,
        ref: ref || null,
        tag: tag,
        type: NodeTypes.ELEMENT
    };
}
function createStaticVElement(tag, props, children) {
    return {
        children: children,
        props: props,
        tag: tag,
        type: NodeTypes.ELEMENT
    };
}
function createVFragment(children, childrenType) {
    return {
        children: children,
        childrenType: childrenType || ChildrenTypes.UNKNOWN,
        dom: null,
        pointer: null,
        type: NodeTypes.FRAGMENT
    };
}
function createVPlaceholder() {
    return {
        dom: null,
        type: NodeTypes.PLACEHOLDER
    };
}
function isVElement(o) {
    return o.type === NodeTypes.ELEMENT;
}
function isOptVElement(o) {
    return o.type === NodeTypes.OPT_ELEMENT;
}
function isVComponent(o) {
    return o.type === NodeTypes.COMPONENT;
}

function convertVOptElementToVElement(optVElement) {
    var bp = optVElement.bp;
    var staticElement = bp.staticVElement;
    var vElement = createVElement(staticElement.tag, null, null, optVElement.key, null, null);
    var bp0 = bp.v0;
    var staticChildren = staticElement.children;
    var staticProps = staticElement.props;
    if (!isNull(staticChildren)) {
        vElement.children = staticChildren;
    }
    if (!isNull(staticProps)) {
        vElement.props = staticProps;
    }
    if (!isNull(bp0)) {
        attachOptVElementValue(vElement, optVElement, bp0, optVElement.v0, bp.d0);
        var bp1 = bp.v1;
        if (!isNull(bp1)) {
            attachOptVElementValue(vElement, optVElement, bp1, optVElement.v1, bp.d1);
            var bp2 = bp.v2;
            if (!isNull(bp2)) {
                attachOptVElementValue(vElement, optVElement, bp2, optVElement.v2, bp.d2);
                var bp3 = bp.v3;
                if (!isNull(bp3)) {
                    var v3 = optVElement.v3;
                    var d3 = bp.d3;
                    var bp3$1 = bp.v3;
                    for (var i = 0; i < bp3$1.length; i++) {
                        attachOptVElementValue(vElement, optVElement, bp3$1[i], v3[i], d3[i]);
                    }
                }
            }
        }
    }
    return vElement;
}
function attachOptVElementValue(vElement, vOptElement, valueType, value, descriptor) {
    switch (valueType) {
        case ValueTypes.CHILDREN:
            vElement.childrenType = descriptor;
            if (isNullOrUndef(vElement.children)) {
                vElement.children = value;
            }
            else {
                debugger;
            }
            break;
        case ValueTypes.PROP_CLASS_NAME:
            if (!vElement.props) {
                vElement.props = { className: value };
            }
            else {
                debugger;
            }
            break;
        case ValueTypes.PROP_DATA:
            if (!vElement.props) {
                vElement.props = {};
            }
            vElement.props['data-' + descriptor] = value;
            break;
        case ValueTypes.PROP_STYLE:
            if (!vElement.props) {
                vElement.props = { style: value };
            }
            else {
                debugger;
            }
            break;
        case ValueTypes.PROP_VALUE:
            if (!vElement.props) {
                vElement.props = { value: value };
            }
            else {
                debugger;
            }
            break;
        case ValueTypes.PROP:
            if (!vElement.props) {
                vElement.props = {};
            }
            vElement.props[descriptor] = value;
            break;
        case ValueTypes.PROP_REF:
            vElement.ref = value;
            break;
        case ValueTypes.PROP_SPREAD:
            if (!vElement.props) {
                vElement.props = value;
            }
            else {
                debugger;
            }
            break;
    }
}
function cloneVNode(vNodeToClone, props) {
    var _children = [], len = arguments.length - 2;
    while ( len-- > 0 ) _children[ len ] = arguments[ len + 2 ];

    var children = _children;
    if (_children.length > 0 && !isNull(_children[0])) {
        if (!props) {
            props = {};
        }
        if (_children.length === 1) {
            children = _children[0];
        }
        if (isUndefined(props.children)) {
            props.children = children;
        }
        else {
            if (isArray(children)) {
                if (isArray(props.children)) {
                    props.children = props.children.concat(children);
                }
                else {
                    props.children = [props.children].concat(children);
                }
            }
            else {
                if (isArray(props.children)) {
                    props.children.push(children);
                }
                else {
                    props.children = [props.children];
                    props.children.push(children);
                }
            }
        }
    }
    else {
        children = null;
    }
    var newVNode;
    if (isArray(vNodeToClone)) {
        newVNode = vNodeToClone.map(function (vNode) { return cloneVNode(vNode); });
    }
    else if (isNullOrUndef(props) && isNullOrUndef(children)) {
        newVNode = Object.assign({}, vNodeToClone);
    }
    else {
        if (isVComponent(vNodeToClone)) {
            newVNode = createVComponent(vNodeToClone.component, Object.assign({}, vNodeToClone.props, props), vNodeToClone.key, vNodeToClone.hooks, vNodeToClone.ref);
        }
        else if (isVElement(vNodeToClone)) {
            newVNode = createVElement(vNodeToClone.tag, Object.assign({}, vNodeToClone.props, props), (props && props.children) || children || vNodeToClone.children, vNodeToClone.key, vNodeToClone.ref, ChildrenTypes.UNKNOWN);
        }
        else if (isOptVElement(vNodeToClone)) {
            newVNode = cloneVNode(convertVOptElementToVElement(vNodeToClone), props, children);
        }
    }
    newVNode.dom = null;
    return newVNode;
}

if ("development" !== 'production') {
	var testFunc = function testFn() {};
	warning(
		(testFunc.name || testFunc.toString()).indexOf('testFn') !== -1,
		'It looks like you\'re using a minified copy of the development build ' +
		'of Inferno. When deploying Inferno apps to production, make sure to use ' +
		'the production build which skips development warnings and is faster. ' +
		'See http://infernojs.org for more details.'
	);
}

var index = {
	// JSX optimisations
	createOptVElement: createOptVElement,
	createOptBlueprint: createOptBlueprint,
	createStaticVElement: createStaticVElement,

	// core shapes
	createVElement: createVElement,
	createVFragment: createVFragment,
	createVPlaceholder: createVPlaceholder,
	createVComponent: createVComponent,
	createVText: createVText,

	// cloning
	cloneVNode: cloneVNode,	

	// enums
	ValueTypes: ValueTypes,
	ChildrenTypes: ChildrenTypes,
	NodeTypes: NodeTypes,

	// TODO do we still need this? can we remove?
	NO_OP: NO_OP
};

return index;

})));
},{}],4:[function(require,module,exports){
'use strict';

module.exports = require('inferno/dist/inferno');
},{"inferno/dist/inferno":3}],5:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Data_Functor = require("../Data.Functor");
var Data_Semigroup = require("../Data.Semigroup");
var Alt = function (__superclass_Data$dotFunctor$dotFunctor_0, alt) {
    this["__superclass_Data.Functor.Functor_0"] = __superclass_Data$dotFunctor$dotFunctor_0;
    this.alt = alt;
};
var altArray = new Alt(function () {
    return Data_Functor.functorArray;
}, Data_Semigroup.append(Data_Semigroup.semigroupArray));
var alt = function (dict) {
    return dict.alt;
};
module.exports = {
    Alt: Alt, 
    alt: alt, 
    altArray: altArray
};

},{"../Data.Functor":100,"../Data.Semigroup":132}],6:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Plus = require("../Control.Plus");
var Data_Functor = require("../Data.Functor");
var Alternative = function (__superclass_Control$dotApplicative$dotApplicative_0, __superclass_Control$dotPlus$dotPlus_1) {
    this["__superclass_Control.Applicative.Applicative_0"] = __superclass_Control$dotApplicative$dotApplicative_0;
    this["__superclass_Control.Plus.Plus_1"] = __superclass_Control$dotPlus$dotPlus_1;
};
var alternativeArray = new Alternative(function () {
    return Control_Applicative.applicativeArray;
}, function () {
    return Control_Plus.plusArray;
});
module.exports = {
    Alternative: Alternative, 
    alternativeArray: alternativeArray
};

},{"../Control.Alt":5,"../Control.Applicative":7,"../Control.Apply":9,"../Control.Plus":52,"../Data.Functor":100}],7:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Apply = require("../Control.Apply");
var Data_Functor = require("../Data.Functor");
var Data_Unit = require("../Data.Unit");
var Applicative = function (__superclass_Control$dotApply$dotApply_0, pure) {
    this["__superclass_Control.Apply.Apply_0"] = __superclass_Control$dotApply$dotApply_0;
    this.pure = pure;
};
var pure = function (dict) {
    return dict.pure;
};
var unless = function (dictApplicative) {
    return function (v) {
        return function (v1) {
            if (!v) {
                return v1;
            };
            if (v) {
                return pure(dictApplicative)(Data_Unit.unit);
            };
            throw new Error("Failed pattern match at Control.Applicative line 63, column 1 - line 63, column 19: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
var when = function (dictApplicative) {
    return function (v) {
        return function (v1) {
            if (v) {
                return v1;
            };
            if (!v) {
                return pure(dictApplicative)(Data_Unit.unit);
            };
            throw new Error("Failed pattern match at Control.Applicative line 58, column 1 - line 58, column 16: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
var liftA1 = function (dictApplicative) {
    return function (f) {
        return function (a) {
            return Control_Apply.apply(dictApplicative["__superclass_Control.Apply.Apply_0"]())(pure(dictApplicative)(f))(a);
        };
    };
};
var applicativeFn = new Applicative(function () {
    return Control_Apply.applyFn;
}, function (x) {
    return function (v) {
        return x;
    };
});
var applicativeArray = new Applicative(function () {
    return Control_Apply.applyArray;
}, function (x) {
    return [ x ];
});
module.exports = {
    Applicative: Applicative, 
    liftA1: liftA1, 
    pure: pure, 
    unless: unless, 
    when: when, 
    applicativeFn: applicativeFn, 
    applicativeArray: applicativeArray
};

},{"../Control.Apply":9,"../Data.Functor":100,"../Data.Unit":151}],8:[function(require,module,exports){
"use strict";

// module Control.Apply

exports.arrayApply = function (fs) {
  return function (xs) {
    var result = [];
    var n = 0;
    for (var i = 0, l = fs.length; i < l; i++) {
      for (var j = 0, k = xs.length; j < k; j++) {
        result[n++] = fs[i](xs[j]);
      }
    }
    return result;
  };
};

},{}],9:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Data_Functor = require("../Data.Functor");
var Data_Function = require("../Data.Function");
var Control_Category = require("../Control.Category");
var Apply = function (__superclass_Data$dotFunctor$dotFunctor_0, apply) {
    this["__superclass_Data.Functor.Functor_0"] = __superclass_Data$dotFunctor$dotFunctor_0;
    this.apply = apply;
};
var applyFn = new Apply(function () {
    return Data_Functor.functorFn;
}, function (f) {
    return function (g) {
        return function (x) {
            return f(x)(g(x));
        };
    };
});
var applyArray = new Apply(function () {
    return Data_Functor.functorArray;
}, $foreign.arrayApply);
var apply = function (dict) {
    return dict.apply;
};
var applyFirst = function (dictApply) {
    return function (a) {
        return function (b) {
            return apply(dictApply)(Data_Functor.map(dictApply["__superclass_Data.Functor.Functor_0"]())(Data_Function["const"])(a))(b);
        };
    };
};
var applySecond = function (dictApply) {
    return function (a) {
        return function (b) {
            return apply(dictApply)(Data_Functor.map(dictApply["__superclass_Data.Functor.Functor_0"]())(Data_Function["const"](Control_Category.id(Control_Category.categoryFn)))(a))(b);
        };
    };
};
var lift2 = function (dictApply) {
    return function (f) {
        return function (a) {
            return function (b) {
                return apply(dictApply)(Data_Functor.map(dictApply["__superclass_Data.Functor.Functor_0"]())(f)(a))(b);
            };
        };
    };
};
var lift3 = function (dictApply) {
    return function (f) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return apply(dictApply)(apply(dictApply)(Data_Functor.map(dictApply["__superclass_Data.Functor.Functor_0"]())(f)(a))(b))(c);
                };
            };
        };
    };
};
var lift4 = function (dictApply) {
    return function (f) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return apply(dictApply)(apply(dictApply)(apply(dictApply)(Data_Functor.map(dictApply["__superclass_Data.Functor.Functor_0"]())(f)(a))(b))(c))(d);
                    };
                };
            };
        };
    };
};
var lift5 = function (dictApply) {
    return function (f) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return function (e) {
                            return apply(dictApply)(apply(dictApply)(apply(dictApply)(apply(dictApply)(Data_Functor.map(dictApply["__superclass_Data.Functor.Functor_0"]())(f)(a))(b))(c))(d))(e);
                        };
                    };
                };
            };
        };
    };
};
module.exports = {
    Apply: Apply, 
    apply: apply, 
    applyFirst: applyFirst, 
    applySecond: applySecond, 
    lift2: lift2, 
    lift3: lift3, 
    lift4: lift4, 
    lift5: lift5, 
    applyFn: applyFn, 
    applyArray: applyArray
};

},{"../Control.Category":14,"../Data.Function":97,"../Data.Functor":100,"./foreign":8}],10:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Biapply = require("../Control.Biapply");
var Biapplicative = function (__superclass_Control$dotBiapply$dotBiapply_0, bipure) {
    this["__superclass_Control.Biapply.Biapply_0"] = __superclass_Control$dotBiapply$dotBiapply_0;
    this.bipure = bipure;
};
var bipure = function (dict) {
    return dict.bipure;
};
module.exports = {
    Biapplicative: Biapplicative, 
    bipure: bipure
};

},{"../Control.Biapply":11}],11:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Data_Function = require("../Data.Function");
var Data_Bifunctor = require("../Data.Bifunctor");
var Control_Category = require("../Control.Category");
var Biapply = function (__superclass_Data$dotBifunctor$dotBifunctor_0, biapply) {
    this["__superclass_Data.Bifunctor.Bifunctor_0"] = __superclass_Data$dotBifunctor$dotBifunctor_0;
    this.biapply = biapply;
};
var biapply = function (dict) {
    return dict.biapply;
};
var biapplyFirst = function (dictBiapply) {
    return function (a) {
        return function (b) {
            return biapply(dictBiapply)(Control_Category.id(Control_Category.categoryFn)(Data_Bifunctor.bimap(dictBiapply["__superclass_Data.Bifunctor.Bifunctor_0"]())(Data_Function["const"](Control_Category.id(Control_Category.categoryFn)))(Data_Function["const"](Control_Category.id(Control_Category.categoryFn))))(a))(b);
        };
    };
};
var biapplySecond = function (dictBiapply) {
    return function (a) {
        return function (b) {
            return biapply(dictBiapply)(Control_Category.id(Control_Category.categoryFn)(Data_Bifunctor.bimap(dictBiapply["__superclass_Data.Bifunctor.Bifunctor_0"]())(Data_Function["const"])(Data_Function["const"]))(a))(b);
        };
    };
};
var bilift2 = function (dictBiapply) {
    return function (f) {
        return function (g) {
            return function (a) {
                return function (b) {
                    return biapply(dictBiapply)(Control_Category.id(Control_Category.categoryFn)(Data_Bifunctor.bimap(dictBiapply["__superclass_Data.Bifunctor.Bifunctor_0"]())(f)(g))(a))(b);
                };
            };
        };
    };
};
var bilift3 = function (dictBiapply) {
    return function (f) {
        return function (g) {
            return function (a) {
                return function (b) {
                    return function (c) {
                        return biapply(dictBiapply)(biapply(dictBiapply)(Control_Category.id(Control_Category.categoryFn)(Data_Bifunctor.bimap(dictBiapply["__superclass_Data.Bifunctor.Bifunctor_0"]())(f)(g))(a))(b))(c);
                    };
                };
            };
        };
    };
};
module.exports = {
    Biapply: Biapply, 
    biapply: biapply, 
    biapplyFirst: biapplyFirst, 
    biapplySecond: biapplySecond, 
    bilift2: bilift2, 
    bilift3: bilift3
};

},{"../Control.Category":14,"../Data.Bifunctor":69,"../Data.Function":97}],12:[function(require,module,exports){
"use strict";

// module Control.Bind

exports.arrayBind = function (arr) {
  return function (f) {
    var result = [];
    for (var i = 0, l = arr.length; i < l; i++) {
      Array.prototype.push.apply(result, f(arr[i]));
    }
    return result;
  };
};

},{}],13:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Category = require("../Control.Category");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Bind = function (__superclass_Control$dotApply$dotApply_0, bind) {
    this["__superclass_Control.Apply.Apply_0"] = __superclass_Control$dotApply$dotApply_0;
    this.bind = bind;
};
var bindFn = new Bind(function () {
    return Control_Apply.applyFn;
}, function (m) {
    return function (f) {
        return function (x) {
            return f(m(x))(x);
        };
    };
});
var bindArray = new Bind(function () {
    return Control_Apply.applyArray;
}, $foreign.arrayBind);
var bind = function (dict) {
    return dict.bind;
};
var bindFlipped = function (dictBind) {
    return Data_Function.flip(bind(dictBind));
};
var composeKleisliFlipped = function (dictBind) {
    return function (f) {
        return function (g) {
            return function (a) {
                return bindFlipped(dictBind)(f)(g(a));
            };
        };
    };
};
var composeKleisli = function (dictBind) {
    return function (f) {
        return function (g) {
            return function (a) {
                return bind(dictBind)(f(a))(g);
            };
        };
    };
};
var ifM = function (dictBind) {
    return function (cond) {
        return function (t) {
            return function (f) {
                return bind(dictBind)(cond)(function (cond$prime) {
                    if (cond$prime) {
                        return t;
                    };
                    if (!cond$prime) {
                        return f;
                    };
                    throw new Error("Failed pattern match at Control.Bind line 103, column 35 - line 103, column 56: " + [ cond$prime.constructor.name ]);
                });
            };
        };
    };
};
var join = function (dictBind) {
    return function (m) {
        return bind(dictBind)(m)(Control_Category.id(Control_Category.categoryFn));
    };
};
module.exports = {
    Bind: Bind, 
    bind: bind, 
    bindFlipped: bindFlipped, 
    composeKleisli: composeKleisli, 
    composeKleisliFlipped: composeKleisliFlipped, 
    ifM: ifM, 
    join: join, 
    bindFn: bindFn, 
    bindArray: bindArray
};

},{"../Control.Applicative":7,"../Control.Apply":9,"../Control.Category":14,"../Data.Function":97,"../Data.Functor":100,"./foreign":12}],14:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Category = function (__superclass_Control$dotSemigroupoid$dotSemigroupoid_0, id) {
    this["__superclass_Control.Semigroupoid.Semigroupoid_0"] = __superclass_Control$dotSemigroupoid$dotSemigroupoid_0;
    this.id = id;
};
var id = function (dict) {
    return dict.id;
};
var categoryFn = new Category(function () {
    return Control_Semigroupoid.semigroupoidFn;
}, function (x) {
    return x;
});
module.exports = {
    Category: Category, 
    id: id, 
    categoryFn: categoryFn
};

},{"../Control.Semigroupoid":53}],15:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Extend = require("../Control.Extend");
var Data_Functor = require("../Data.Functor");
var Comonad = function (__superclass_Control$dotExtend$dotExtend_0, extract) {
    this["__superclass_Control.Extend.Extend_0"] = __superclass_Control$dotExtend$dotExtend_0;
    this.extract = extract;
};
var extract = function (dict) {
    return dict.extract;
};
module.exports = {
    Comonad: Comonad, 
    extract: extract
};

},{"../Control.Extend":16,"../Data.Functor":100}],16:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Category = require("../Control.Category");
var Data_Functor = require("../Data.Functor");
var Data_Semigroup = require("../Data.Semigroup");
var Extend = function (__superclass_Data$dotFunctor$dotFunctor_0, extend) {
    this["__superclass_Data.Functor.Functor_0"] = __superclass_Data$dotFunctor$dotFunctor_0;
    this.extend = extend;
};
var extendFn = function (dictSemigroup) {
    return new Extend(function () {
        return Data_Functor.functorFn;
    }, function (f) {
        return function (g) {
            return function (w) {
                return f(function (w$prime) {
                    return g(Data_Semigroup.append(dictSemigroup)(w)(w$prime));
                });
            };
        };
    });
};
var extend = function (dict) {
    return dict.extend;
};
var extendFlipped = function (dictExtend) {
    return function (w) {
        return function (f) {
            return extend(dictExtend)(f)(w);
        };
    };
};
var duplicate = function (dictExtend) {
    return extend(dictExtend)(Control_Category.id(Control_Category.categoryFn));
};
var composeCoKleisliFlipped = function (dictExtend) {
    return function (f) {
        return function (g) {
            return function (w) {
                return f(extend(dictExtend)(g)(w));
            };
        };
    };
};
var composeCoKleisli = function (dictExtend) {
    return function (f) {
        return function (g) {
            return function (w) {
                return g(extend(dictExtend)(f)(w));
            };
        };
    };
};
module.exports = {
    Extend: Extend, 
    composeCoKleisli: composeCoKleisli, 
    composeCoKleisliFlipped: composeCoKleisliFlipped, 
    duplicate: duplicate, 
    extend: extend, 
    extendFlipped: extendFlipped, 
    extendFn: extendFn
};

},{"../Control.Category":14,"../Data.Functor":100,"../Data.Semigroup":132}],17:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Data_Unit = require("../Data.Unit");
var Lazy = function (defer) {
    this.defer = defer;
};
var defer = function (dict) {
    return dict.defer;
};
var fix = function (dictLazy) {
    return function (f) {
        return defer(dictLazy)(function (v) {
            return f(fix(dictLazy)(f));
        });
    };
};
module.exports = {
    Lazy: Lazy, 
    defer: defer, 
    fix: fix
};

},{"../Data.Unit":151}],18:[function(require,module,exports){
/* global exports */
"use strict";

exports._makeVar = function (nonCanceler) {
  return function(success, error) {
    try {
      success({
        consumers: [],
        producers: [],
        error: undefined
      });
    } catch (err) {
      error(err);
    }

    return nonCanceler;
  };
};

exports._takeVar = function (nonCanceler, avar) {
  return function(success, error) {
    if (avar.error !== undefined) {
      error(avar.error);
    } else if (avar.producers.length > 0) {
      var producer = avar.producers.shift();

      producer(success, error);
    } else {
      avar.consumers.push({success: success, error: error});
    }

    return nonCanceler;
  };
};

exports._putVar = function (nonCanceler, avar, a) {
  return function(success, error) {
    if (avar.error !== undefined) {
      error(avar.error);
    } else if (avar.consumers.length === 0) {
      avar.producers.push(function(success, error) {
        try {
          success(a);
        } catch (err) {
          error(err);
        }
      });

      success({});
    } else {
      var consumer = avar.consumers.shift();

      try {
        consumer.success(a);
      } catch (err) {
        error(err);

        return;
      }

      success({});
    }

    return nonCanceler;
  };
};

exports._killVar = function (nonCanceler, avar, e) {
  return function(success, error) {
    if (avar.error !== undefined) {
      error(avar.error);
    } else {
      var errors = [];

      avar.error = e;

      while (avar.consumers.length > 0) {
        var consumer = avar.consumers.shift();

        try {
          consumer.error(e);
        } catch (err) {
          errors.push(err);
        }
      }

      if (errors.length > 0) error(errors[0]);
      else success({});
    }

    return nonCanceler;
  };
};

},{}],19:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var Data_Function_Uncurried = require("../Data.Function.Uncurried");
module.exports = {
    _killVar: $foreign._killVar, 
    _makeVar: $foreign._makeVar, 
    _putVar: $foreign._putVar, 
    _takeVar: $foreign._takeVar
};

},{"../Control.Monad.Eff.Exception":28,"../Data.Function.Uncurried":96,"../Prelude":162,"./foreign":18}],20:[function(require,module,exports){
/* global exports */
"use strict";

exports._cancelWith = function (nonCanceler, aff, canceler1) {
  return function(success, error) {
    var canceler2 = aff(success, error);

    return function(e) {
      return function(success, error) {
        var cancellations = 0;
        var result        = false;
        var errored       = false;

        var s = function(bool) {
          cancellations = cancellations + 1;
          result        = result || bool;

          if (cancellations === 2 && !errored) {
            success(result);
          }
        };

        var f = function(err) {
          if (!errored) {
            errored = true;
            error(err);
          }
        };

        canceler2(e)(s, f);
        canceler1(e)(s, f);

        return nonCanceler;
      };
    };
  };
}

exports._setTimeout = function (nonCanceler, millis, aff) {
  var set = setTimeout, clear = clearTimeout;
  if (millis <= 0 && typeof setImmediate === "function") {
    set = setImmediate;
    clear = clearImmediate;
  }
  return function(success, error) {
    var canceler;

    var timeout = set(function() {
      canceler = aff(success, error);
    }, millis);

    return function(e) {
      return function(s, f) {
        if (canceler !== undefined) {
          return canceler(e)(s, f);
        } else {
          clear(timeout);
          s(true);
          return nonCanceler;
        }
      };
    };
  };
}

exports._unsafeInterleaveAff = function (aff) {
  return aff;
}

exports._forkAff = function (nonCanceler, aff) {
  var voidF = function(){};

  return function(success, error) {
    var canceler = aff(voidF, voidF);
    success(canceler);
    return nonCanceler;
  };
}

exports._forkAll = function (nonCanceler, foldl, affs) {
  var voidF = function(){};

  return function(success, error) {
    try {
      var cancelers = foldl(function(acc) {
        return function(aff) {
          acc.push(aff(voidF, voidF));
          return acc;
        }
      })([])(affs);
    } catch (err) {
      error(err)
    }

    var canceler = function(e) {
      return function(success, error) {
        var cancellations = 0;
        var result        = false;
        var errored       = false;

        var s = function(bool) {
          cancellations = cancellations + 1;
          result        = result || bool;

          if (cancellations === cancelers.length && !errored) {
            success(result);
          }
        };

        var f = function(err) {
          if (!errored) {
            errored = true;
            error(err);
          }
        };

        for (var i = 0; i < cancelers.length; i++) {
          cancelers[i](e)(s, f);
        }

        return nonCanceler;
      };
    };

    success(canceler);
    return nonCanceler;
  };
}

exports._makeAff = function (cb) {
  return function(success, error) {
    try {
      return cb(function(e) {
        return function() {
          error(e);
        };
      })(function(v) {
        return function() {
          success(v);
        };
      })();
    } catch (err) {
      error(err);
    }
  }
}

exports._pure = function (nonCanceler, v) {
  return function(success, error) {
    success(v);
    return nonCanceler;
  };
}

exports._throwError = function (nonCanceler, e) {
  return function(success, error) {
    error(e);
    return nonCanceler;
  };
}

exports._fmap = function (f, aff) {
  return function(success, error) {
    try {
      return aff(function(v) {
        try {
          var v2 = f(v);
        } catch (err) {
          error(err)
        }
        success(v2);
      }, error);
    } catch (err) {
      error(err);
    }
  };
}

exports._bind = function (alwaysCanceler, aff, f) {
  return function(success, error) {
    var canceler1, canceler2;

    var isCanceled    = false;
    var requestCancel = false;

    var onCanceler = function(){};

    canceler1 = aff(function(v) {
      if (requestCancel) {
        isCanceled = true;

        return alwaysCanceler;
      } else {
        canceler2 = f(v)(success, error);

        onCanceler(canceler2);

        return canceler2;
      }
    }, error);

    return function(e) {
      return function(s, f) {
        requestCancel = true;

        if (canceler2 !== undefined) {
          return canceler2(e)(s, f);
        } else {
          return canceler1(e)(function(bool) {
            if (bool || isCanceled) {
              s(true);
            } else {
              onCanceler = function(canceler) {
                canceler(e)(s, f);
              };
            }
          }, f);
        }
      };
    };
  };
}

exports._attempt = function (Left, Right, aff) {
  return function(success, error) {
    try {
      return aff(function(v) {
        success(Right(v));
      }, function(e) {
        success(Left(e));
      });
    } catch (err) {
      success(Left(err));
    }
  };
}

exports._runAff = function (errorT, successT, aff) {
  return function() {
    return aff(function(v) {
      successT(v)();
    }, function(e) {
      errorT(e)();
    });
  };
}

exports._liftEff = function (nonCanceler, e) {
  return function(success, error) {
    var result;
    try {
      result = e();
    } catch (err) {
      error(err);
      return nonCanceler;
    }

    success(result);
    return nonCanceler;
  };
}

exports._tailRecM = function (isLeft, f, a) {
  return function(success, error) {
    return function go(acc) {
      var result, status, canceler;

      // Observes synchronous effects using a flag.
      //   status = 0 (unresolved status)
      //   status = 1 (synchronous effect)
      //   status = 2 (asynchronous effect)
      while (true) {
        status = 0;
        canceler = f(acc)(function(v) {
          // If the status is still unresolved, we have observed a
          // synchronous effect. Otherwise, the status will be `2`.
          if (status === 0) {
            // Store the result for further synchronous processing.
            result = v;
            status = 1;
          } else {
            // When we have observed an asynchronous effect, we use normal
            // recursion. This is safe because we will be on a new stack.
            if (isLeft(v)) {
              go(v.value0);
            } else {
              try {
                success(v.value0);
              } catch (err) {
                error(err);
              }
            }
          }
        }, error);

        // If the status has already resolved to `1` by our Aff handler, then
        // we have observed a synchronous effect. Otherwise it will still be
        // `0`.
        if (status === 1) {
          // When we have observed a synchronous effect, we merely swap out the
          // accumulator and continue the loop, preserving stack.
          if (isLeft(result)) {
            acc = result.value0;
            continue;
          } else {
            try {
              success(result.value0);
            } catch (err) {
              error(err);
            }
          }
        } else {
          // If the status has not resolved yet, then we have observed an
          // asynchronous effect.
          status = 2;
        }
        return canceler;
      }

    }(a);
  };
};

},{}],21:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Monad_Aff_Internal = require("../Control.Monad.Aff.Internal");
var Control_Monad_Cont_Class = require("../Control.Monad.Cont.Class");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_MonadPlus = require("../Control.MonadPlus");
var Control_Parallel_Class = require("../Control.Parallel.Class");
var Control_Plus = require("../Control.Plus");
var Data_Either = require("../Data.Either");
var Data_Foldable = require("../Data.Foldable");
var Data_Function_Uncurried = require("../Data.Function.Uncurried");
var Data_Monoid = require("../Data.Monoid");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var Data_Semigroup = require("../Data.Semigroup");
var Control_Apply = require("../Control.Apply");
var Data_Functor = require("../Data.Functor");
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad = require("../Control.Monad");
var Data_Function = require("../Data.Function");
var Control_MonadZero = require("../Control.MonadZero");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Eq = require("../Data.Eq");
var Data_Unit = require("../Data.Unit");
var Data_Semiring = require("../Data.Semiring");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Canceler = function (x) {
    return x;
};
var runAff = function (ex) {
    return function (f) {
        return function (aff) {
            return $foreign._runAff(ex, f, aff);
        };
    };
};
var makeAff$prime = function (h) {
    return $foreign._makeAff(h);
};
var functorAff = new Data_Functor.Functor(function (f) {
    return function (fa) {
        return $foreign._fmap(f, fa);
    };
});
var fromAVBox = Unsafe_Coerce.unsafeCoerce;
var cancel = function (v) {
    return v;
};
var launchAff = (function () {
    var lowerEx = Data_Functor.map(Control_Monad_Eff.functorEff)(function ($34) {
        return Canceler(Data_Functor.map(Data_Functor.functorFn)($foreign._unsafeInterleaveAff)(cancel($34)));
    });
    return function ($35) {
        return lowerEx(runAff(Control_Monad_Eff_Exception.throwException)(Data_Function["const"](Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(Data_Unit.unit)))($foreign._unsafeInterleaveAff($35)));
    };
})();
var attempt = function (aff) {
    return $foreign._attempt(Data_Either.Left.create, Data_Either.Right.create, aff);
};
var apathize = function (a) {
    return Data_Functor.map(functorAff)(Data_Function["const"](Data_Unit.unit))(attempt(a));
};
var applyAff = new Control_Apply.Apply(function () {
    return functorAff;
}, function (ff) {
    return function (fa) {
        return $foreign._bind(alwaysCanceler, ff, function (f) {
            return Data_Functor.map(functorAff)(f)(fa);
        });
    };
});
var applicativeAff = new Control_Applicative.Applicative(function () {
    return applyAff;
}, function (v) {
    return $foreign._pure(nonCanceler, v);
});
var nonCanceler = Data_Function["const"](Control_Applicative.pure(applicativeAff)(false));
var alwaysCanceler = Data_Function["const"](Control_Applicative.pure(applicativeAff)(true));
var cancelWith = function (aff) {
    return function (c) {
        return $foreign._cancelWith(nonCanceler, aff, c);
    };
};
var forkAff = function (aff) {
    return $foreign._forkAff(nonCanceler, aff);
};
var forkAll = function (dictFoldable) {
    return function (affs) {
        return $foreign._forkAll(nonCanceler, Data_Foldable.foldl(dictFoldable), affs);
    };
};
var killVar = function (q) {
    return function (e) {
        return Data_Function.apply(fromAVBox)(Control_Monad_Aff_Internal._killVar(nonCanceler, q, e));
    };
};
var later$prime = function (n) {
    return function (aff) {
        return $foreign._setTimeout(nonCanceler, n, aff);
    };
};
var later = later$prime(0);
var liftEff$prime = function (eff) {
    return attempt($foreign._unsafeInterleaveAff($foreign._liftEff(nonCanceler, eff)));
};
var makeAff = function (h) {
    return makeAff$prime(function (e) {
        return function (a) {
            return Data_Functor.map(Control_Monad_Eff.functorEff)(Data_Function["const"](nonCanceler))(h(e)(a));
        };
    });
};
var makeVar = Data_Function.apply(fromAVBox)(Control_Monad_Aff_Internal._makeVar(nonCanceler));
var putVar = function (q) {
    return function (a) {
        return Data_Function.apply(fromAVBox)(Control_Monad_Aff_Internal._putVar(nonCanceler, q, a));
    };
};
var takeVar = function (q) {
    return Data_Function.apply(fromAVBox)(Control_Monad_Aff_Internal._takeVar(nonCanceler, q));
};
var semigroupAff = function (dictSemigroup) {
    return new Data_Semigroup.Semigroup(function (a) {
        return function (b) {
            return Control_Apply.apply(applyAff)(Data_Functor.map(functorAff)(Data_Semigroup.append(dictSemigroup))(a))(b);
        };
    });
};
var monoidAff = function (dictMonoid) {
    return new Data_Monoid.Monoid(function () {
        return semigroupAff(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]());
    }, Control_Applicative.pure(applicativeAff)(Data_Monoid.mempty(dictMonoid)));
};
var semigroupCanceler = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        return function (e) {
            return Control_Apply.apply(applyAff)(Data_Functor.map(functorAff)(Data_HeytingAlgebra.disj(Data_HeytingAlgebra.heytingAlgebraBoolean))(v(e)))(v1(e));
        };
    };
});
var monoidCanceler = new Data_Monoid.Monoid(function () {
    return semigroupCanceler;
}, Data_Function["const"](Control_Applicative.pure(applicativeAff)(true)));
var bindAff = new Control_Bind.Bind(function () {
    return applyAff;
}, function (fa) {
    return function (f) {
        return $foreign._bind(alwaysCanceler, fa, f);
    };
});
var monadAff = new Control_Monad.Monad(function () {
    return applicativeAff;
}, function () {
    return bindAff;
});
var monadContAff = new Control_Monad_Cont_Class.MonadCont(function () {
    return monadAff;
}, function (f) {
    return makeAff(function (eb) {
        return function (cb) {
            return Data_Function.apply(Data_Functor["void"](Control_Monad_Eff.functorEff))(runAff(eb)(cb)(f(function (a) {
                return makeAff(function (v) {
                    return function (v1) {
                        return cb(a);
                    };
                });
            })));
        };
    });
});
var monadEffAff = new Control_Monad_Eff_Class.MonadEff(function () {
    return monadAff;
}, function (eff) {
    return $foreign._liftEff(nonCanceler, eff);
});
var monadRecAff = new Control_Monad_Rec_Class.MonadRec(function () {
    return monadAff;
}, function (f) {
    return function (a) {
        return $foreign._tailRecM(Data_Either.isLeft, f, a);
    };
});
var monadErrorAff = new Control_Monad_Error_Class.MonadError(function () {
    return monadAff;
}, function (aff) {
    return function (ex) {
        return Control_Bind.bind(bindAff)(attempt(aff))(Data_Either.either(ex)(Control_Applicative.pure(applicativeAff)));
    };
}, function (e) {
    return $foreign._throwError(nonCanceler, e);
});
var $$finally = function (aff1) {
    return function (aff2) {
        return Control_Bind.bind(bindAff)(attempt(aff1))(function (v) {
            return Control_Bind.bind(bindAff)(aff2)(function () {
                return Data_Either.either(Control_Monad_Error_Class.throwError(monadErrorAff))(Control_Applicative.pure(applicativeAff))(v);
            });
        });
    };
};
var monadParAff = new Control_Parallel_Class.MonadPar(function () {
    return monadAff;
}, function (f) {
    return function (ma) {
        return function (mb) {
            var putOrKill = function (v) {
                return Data_Either.either(killVar(v))(putVar(v));
            };
            return Control_Bind.bind(bindAff)(makeVar)(function (v) {
                return Control_Bind.bind(bindAff)(makeVar)(function (v1) {
                    return Control_Bind.bind(bindAff)(forkAff(Control_Bind.bindFlipped(bindAff)(putOrKill(v))(attempt(ma))))(function (v2) {
                        return Control_Bind.bind(bindAff)(forkAff(Control_Bind.bindFlipped(bindAff)(putOrKill(v1))(attempt(mb))))(function (v3) {
                            return Control_Apply.apply(applyAff)(Data_Functor.map(functorAff)(f)(takeVar(v)))(takeVar(v1));
                        });
                    });
                });
            });
        };
    };
});
var monadRaceAff = new Control_Parallel_Class.MonadRace(function () {
    return monadParAff;
}, function (a1) {
    return function (a2) {
        var maybeKill = function (va) {
            return function (ve) {
                return function (err) {
                    return Control_Bind.bind(bindAff)(takeVar(ve))(function (v) {
                        return Control_Bind.bind(bindAff)((function () {
                            var $29 = v === 1;
                            if ($29) {
                                return killVar(va)(err);
                            };
                            if (!$29) {
                                return Control_Applicative.pure(applicativeAff)(Data_Unit.unit);
                            };
                            throw new Error("Failed pattern match at Control.Monad.Aff line 240, column 7 - line 240, column 51: " + [ $29.constructor.name ]);
                        })())(function () {
                            return putVar(ve)(v + 1 | 0);
                        });
                    });
                };
            };
        };
        return Control_Bind.bind(bindAff)(makeVar)(function (v) {
            return Control_Bind.bind(bindAff)(makeVar)(function (v1) {
                return Control_Bind.bind(bindAff)(putVar(v1)(0))(function () {
                    return Control_Bind.bind(bindAff)(Data_Function.apply(forkAff)(Control_Bind.bindFlipped(bindAff)(Data_Either.either(maybeKill(v)(v1))(putVar(v)))(attempt(a1))))(function (v2) {
                        return Control_Bind.bind(bindAff)(Data_Function.apply(forkAff)(Control_Bind.bindFlipped(bindAff)(Data_Either.either(maybeKill(v)(v1))(putVar(v)))(attempt(a2))))(function (v3) {
                            return cancelWith(takeVar(v))(Data_Semigroup.append(semigroupCanceler)(v2)(v3));
                        });
                    });
                });
            });
        });
    };
}, Data_Function.apply(Control_Monad_Error_Class.throwError(monadErrorAff))(Control_Monad_Eff_Exception.error("Stalled")));
var altAff = new Control_Alt.Alt(function () {
    return functorAff;
}, function (a1) {
    return function (a2) {
        return Control_Bind.bind(bindAff)(attempt(a1))(Data_Either.either(Data_Function["const"](a2))(Control_Applicative.pure(applicativeAff)));
    };
});
var plusAff = new Control_Plus.Plus(function () {
    return altAff;
}, Data_Function.apply(Control_Monad_Error_Class.throwError(monadErrorAff))(Control_Monad_Eff_Exception.error("Always fails")));
var alternativeAff = new Control_Alternative.Alternative(function () {
    return applicativeAff;
}, function () {
    return plusAff;
});
var monadZero = new Control_MonadZero.MonadZero(function () {
    return alternativeAff;
}, function () {
    return monadAff;
});
var monadPlusAff = new Control_MonadPlus.MonadPlus(function () {
    return monadZero;
});
module.exports = {
    Canceler: Canceler, 
    apathize: apathize, 
    attempt: attempt, 
    cancel: cancel, 
    cancelWith: cancelWith, 
    "finally": $$finally, 
    forkAff: forkAff, 
    forkAll: forkAll, 
    later: later, 
    "later'": later$prime, 
    launchAff: launchAff, 
    "liftEff'": liftEff$prime, 
    makeAff: makeAff, 
    "makeAff'": makeAff$prime, 
    nonCanceler: nonCanceler, 
    runAff: runAff, 
    semigroupAff: semigroupAff, 
    monoidAff: monoidAff, 
    functorAff: functorAff, 
    applyAff: applyAff, 
    applicativeAff: applicativeAff, 
    bindAff: bindAff, 
    monadAff: monadAff, 
    monadEffAff: monadEffAff, 
    monadErrorAff: monadErrorAff, 
    altAff: altAff, 
    plusAff: plusAff, 
    alternativeAff: alternativeAff, 
    monadZero: monadZero, 
    monadPlusAff: monadPlusAff, 
    monadRecAff: monadRecAff, 
    monadContAff: monadContAff, 
    semigroupCanceler: semigroupCanceler, 
    monoidCanceler: monoidCanceler, 
    monadParAff: monadParAff, 
    monadRaceAff: monadRaceAff
};

},{"../Control.Alt":5,"../Control.Alternative":6,"../Control.Applicative":7,"../Control.Apply":9,"../Control.Bind":13,"../Control.Monad":48,"../Control.Monad.Aff.Internal":19,"../Control.Monad.Cont.Class":22,"../Control.Monad.Eff":34,"../Control.Monad.Eff.Class":24,"../Control.Monad.Eff.Exception":28,"../Control.Monad.Error.Class":35,"../Control.Monad.Rec.Class":41,"../Control.MonadPlus":49,"../Control.MonadZero":50,"../Control.Parallel.Class":51,"../Control.Plus":52,"../Control.Semigroupoid":53,"../Data.Either":77,"../Data.Eq":79,"../Data.Foldable":84,"../Data.Function":97,"../Data.Function.Uncurried":96,"../Data.Functor":100,"../Data.HeytingAlgebra":104,"../Data.Monoid":120,"../Data.Semigroup":132,"../Data.Semiring":134,"../Data.Unit":151,"../Prelude":162,"../Unsafe.Coerce":165,"./foreign":20}],22:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var MonadCont = function (__superclass_Control$dotMonad$dotMonad_0, callCC) {
    this["__superclass_Control.Monad.Monad_0"] = __superclass_Control$dotMonad$dotMonad_0;
    this.callCC = callCC;
};
var callCC = function (dict) {
    return dict.callCC;
};
module.exports = {
    MonadCont: MonadCont, 
    callCC: callCC
};

},{"../Prelude":162}],23:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Control_Monad_Cont_Class = require("../Control.Monad.Cont.Class");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Control_Monad_Trans = require("../Control.Monad.Trans");
var Data_Functor = require("../Data.Functor");
var Data_Function = require("../Data.Function");
var Control_Apply = require("../Control.Apply");
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad = require("../Control.Monad");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var ContT = function (x) {
    return x;
};
var withContT = function (f) {
    return function (v) {
        return function (k) {
            return v(f(k));
        };
    };
};
var runContT = function (v) {
    return function (k) {
        return v(k);
    };
};
var monadTransContT = new Control_Monad_Trans.MonadTrans(function (dictMonad) {
    return function (m) {
        return function (k) {
            return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(m)(k);
        };
    };
});
var mapContT = function (f) {
    return function (v) {
        return function (k) {
            return f(v(k));
        };
    };
};
var functorContT = function (dictFunctor) {
    return new Data_Functor.Functor(function (f) {
        return function (v) {
            return function (k) {
                return v(function (a) {
                    return Data_Function.apply(k)(f(a));
                });
            };
        };
    });
};
var applyContT = function (dictApply) {
    return new Control_Apply.Apply(function () {
        return functorContT(dictApply["__superclass_Data.Functor.Functor_0"]());
    }, function (v) {
        return function (v1) {
            return function (k) {
                return v(function (g) {
                    return v1(function (a) {
                        return k(g(a));
                    });
                });
            };
        };
    });
};
var bindContT = function (dictBind) {
    return new Control_Bind.Bind(function () {
        return applyContT(dictBind["__superclass_Control.Apply.Apply_0"]());
    }, function (v) {
        return function (k) {
            return function (k$prime) {
                return v(function (a) {
                    var $32 = k(a);
                    return $32(k$prime);
                });
            };
        };
    });
};
var applicativeContT = function (dictApplicative) {
    return new Control_Applicative.Applicative(function () {
        return applyContT(dictApplicative["__superclass_Control.Apply.Apply_0"]());
    }, function (a) {
        return function (k) {
            return k(a);
        };
    });
};
var monadContT = function (dictMonad) {
    return new Control_Monad.Monad(function () {
        return applicativeContT(dictMonad["__superclass_Control.Applicative.Applicative_0"]());
    }, function () {
        return bindContT(dictMonad["__superclass_Control.Bind.Bind_1"]());
    });
};
var monadContContT = function (dictMonad) {
    return new Control_Monad_Cont_Class.MonadCont(function () {
        return monadContT(dictMonad);
    }, function (f) {
        return function (k) {
            var $34 = f(function (a) {
                return function (v) {
                    return k(a);
                };
            });
            return $34(k);
        };
    });
};
var monadEffContT = function (dictMonadEff) {
    return new Control_Monad_Eff_Class.MonadEff(function () {
        return monadContT(dictMonadEff["__superclass_Control.Monad.Monad_0"]());
    }, function ($38) {
        return Control_Monad_Trans.lift(monadTransContT)(dictMonadEff["__superclass_Control.Monad.Monad_0"]())(Control_Monad_Eff_Class.liftEff(dictMonadEff)($38));
    });
};
var monadReaderContT = function (dictMonadReader) {
    return new Control_Monad_Reader_Class.MonadReader(function () {
        return monadContT(dictMonadReader["__superclass_Control.Monad.Monad_0"]());
    }, Control_Monad_Trans.lift(monadTransContT)(dictMonadReader["__superclass_Control.Monad.Monad_0"]())(Control_Monad_Reader_Class.ask(dictMonadReader)), function (f) {
        return function (v) {
            return function (k) {
                return Control_Bind.bind((dictMonadReader["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Bind.Bind_1"]())(Control_Monad_Reader_Class.ask(dictMonadReader))(function (v1) {
                    return Control_Monad_Reader_Class.local(dictMonadReader)(f)(v(function ($39) {
                        return Control_Monad_Reader_Class.local(dictMonadReader)(Data_Function["const"](v1))(k($39));
                    }));
                });
            };
        };
    });
};
var monadStateContT = function (dictMonadState) {
    return new Control_Monad_State_Class.MonadState(function () {
        return monadContT(dictMonadState["__superclass_Control.Monad.Monad_0"]());
    }, function ($40) {
        return Control_Monad_Trans.lift(monadTransContT)(dictMonadState["__superclass_Control.Monad.Monad_0"]())(Control_Monad_State_Class.state(dictMonadState)($40));
    });
};
module.exports = {
    ContT: ContT, 
    mapContT: mapContT, 
    runContT: runContT, 
    withContT: withContT, 
    monadContContT: monadContContT, 
    functorContT: functorContT, 
    applyContT: applyContT, 
    applicativeContT: applicativeContT, 
    bindContT: bindContT, 
    monadContT: monadContT, 
    monadTransContT: monadTransContT, 
    monadEffContT: monadEffContT, 
    monadReaderContT: monadReaderContT, 
    monadStateContT: monadStateContT
};

},{"../Control.Applicative":7,"../Control.Apply":9,"../Control.Bind":13,"../Control.Monad":48,"../Control.Monad.Cont.Class":22,"../Control.Monad.Eff.Class":24,"../Control.Monad.Reader.Class":39,"../Control.Monad.State.Class":44,"../Control.Monad.Trans":45,"../Control.Semigroupoid":53,"../Data.Function":97,"../Data.Functor":100,"../Prelude":162}],24:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Category = require("../Control.Category");
var Control_Monad = require("../Control.Monad");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var MonadEff = function (__superclass_Control$dotMonad$dotMonad_0, liftEff) {
    this["__superclass_Control.Monad.Monad_0"] = __superclass_Control$dotMonad$dotMonad_0;
    this.liftEff = liftEff;
};
var monadEffEff = new MonadEff(function () {
    return Control_Monad_Eff.monadEff;
}, Control_Category.id(Control_Category.categoryFn));
var liftEff = function (dict) {
    return dict.liftEff;
};
module.exports = {
    MonadEff: MonadEff, 
    liftEff: liftEff, 
    monadEffEff: monadEffEff
};

},{"../Control.Category":14,"../Control.Monad":48,"../Control.Monad.Eff":34}],25:[function(require,module,exports){
"use strict";

// module Control.Monad.Eff.Console

exports.log = function (s) {
  return function () {
    console.log(s);
    return {};
  };
};

exports.warn = function (s) {
  return function () {
    console.warn(s);
    return {};
  };
};

exports.error = function (s) {
  return function () {
    console.error(s);
    return {};
  };
};

exports.info = function (s) {
  return function () {
    console.info(s);
    return {};
  };
};

},{}],26:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Data_Show = require("../Data.Show");
var Data_Unit = require("../Data.Unit");
var warnShow = function (dictShow) {
    return function (a) {
        return $foreign.warn(Data_Show.show(dictShow)(a));
    };
};
var logShow = function (dictShow) {
    return function (a) {
        return $foreign.log(Data_Show.show(dictShow)(a));
    };
};
var infoShow = function (dictShow) {
    return function (a) {
        return $foreign.info(Data_Show.show(dictShow)(a));
    };
};
var errorShow = function (dictShow) {
    return function (a) {
        return $foreign.error(Data_Show.show(dictShow)(a));
    };
};
module.exports = {
    errorShow: errorShow, 
    infoShow: infoShow, 
    logShow: logShow, 
    warnShow: warnShow, 
    error: $foreign.error, 
    info: $foreign.info, 
    log: $foreign.log, 
    warn: $foreign.warn
};

},{"../Control.Monad.Eff":34,"../Data.Show":136,"../Data.Unit":151,"./foreign":25}],27:[function(require,module,exports){
/* global exports */
"use strict";

// module Control.Monad.Eff.Exception

exports.showErrorImpl = function (err) {
  return err.stack || err.toString();
};

exports.error = function (msg) {
  return new Error(msg);
};

exports.message = function (e) {
  return e.message;
};

exports.stackImpl = function (just) {
  return function (nothing) {
    return function (e) {
      return e.stack ? just(e.stack) : nothing;
    };
  };
};

exports.throwException = function (e) {
  return function () {
    throw e;
  };
};

exports.catchException = function (c) {
  return function (t) {
    return function () {
      try {
        return t();
      } catch (e) {
        if (e instanceof Error || Object.prototype.toString.call(e) === "[object Error]") {
          return c(e)();
        } else {
          return c(new Error(e.toString()))();
        }
      }
    };
  };
};

},{}],28:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Either = require("../Data.Either");
var Data_Maybe = require("../Data.Maybe");
var Data_Show = require("../Data.Show");
var Prelude = require("../Prelude");
var Control_Applicative = require("../Control.Applicative");
var Data_Functor = require("../Data.Functor");
var $$try = function (action) {
    return $foreign.catchException(function ($0) {
        return Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(Data_Either.Left.create($0));
    })(Data_Functor.map(Control_Monad_Eff.functorEff)(Data_Either.Right.create)(action));
};
var $$throw = function ($1) {
    return $foreign.throwException($foreign.error($1));
};
var stack = $foreign.stackImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var showError = new Data_Show.Show($foreign.showErrorImpl);
module.exports = {
    stack: stack, 
    "throw": $$throw, 
    "try": $$try, 
    showError: showError, 
    catchException: $foreign.catchException, 
    error: $foreign.error, 
    message: $foreign.message, 
    throwException: $foreign.throwException
};

},{"../Control.Applicative":7,"../Control.Monad.Eff":34,"../Control.Semigroupoid":53,"../Data.Either":77,"../Data.Functor":100,"../Data.Maybe":113,"../Data.Show":136,"../Prelude":162,"./foreign":27}],29:[function(require,module,exports){
/* global exports */
"use strict";

// module Control.Monad.Eff.Ref

exports.newRef = function (val) {
  return function () {
    return { value: val };
  };
};

exports.readRef = function (ref) {
  return function () {
    return ref.value;
  };
};

exports["modifyRef'"] = function (ref) {
  return function (f) {
    return function () {
      var t = f(ref.value);
      ref.value = t.state;
      return t.value;
    };
  };
};

exports.writeRef = function (ref) {
  return function (val) {
    return function () {
      ref.value = val;
      return {};
    };
  };
};

},{}],30:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Data_Unit = require("../Data.Unit");
var modifyRef = function (ref) {
    return function (f) {
        return $foreign["modifyRef'"](ref)(function (s) {
            return {
                state: f(s), 
                value: Data_Unit.unit
            };
        });
    };
};
module.exports = {
    modifyRef: modifyRef, 
    "modifyRef'": $foreign["modifyRef'"], 
    newRef: $foreign.newRef, 
    readRef: $foreign.readRef, 
    writeRef: $foreign.writeRef
};

},{"../Control.Monad.Eff":34,"../Data.Unit":151,"../Prelude":162,"./foreign":29}],31:[function(require,module,exports){
"use strict";

// module Control.Monad.Eff.Unsafe

exports.unsafeInterleaveEff = function (f) {
  return f;
};

},{}],32:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var unsafePerformEff = function ($0) {
    return Control_Monad_Eff.runPure($foreign.unsafeInterleaveEff($0));
};
module.exports = {
    unsafePerformEff: unsafePerformEff, 
    unsafeInterleaveEff: $foreign.unsafeInterleaveEff
};

},{"../Control.Monad.Eff":34,"../Control.Semigroupoid":53,"./foreign":31}],33:[function(require,module,exports){
"use strict";

// module Control.Monad.Eff

exports.pureE = function (a) {
  return function () {
    return a;
  };
};

exports.bindE = function (a) {
  return function (f) {
    return function () {
      return f(a())();
    };
  };
};

exports.runPure = function (f) {
  return f();
};

exports.untilE = function (f) {
  return function () {
    while (!f());
    return {};
  };
};

exports.whileE = function (f) {
  return function (a) {
    return function () {
      while (f()) {
        a();
      }
      return {};
    };
  };
};

exports.forE = function (lo) {
  return function (hi) {
    return function (f) {
      return function () {
        for (var i = lo; i < hi; i++) {
          f(i)();
        }
      };
    };
  };
};

exports.foreachE = function (as) {
  return function (f) {
    return function () {
      for (var i = 0, l = as.length; i < l; i++) {
        f(as[i])();
      }
    };
  };
};

},{}],34:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Monad = require("../Control.Monad");
var Data_Functor = require("../Data.Functor");
var Data_Unit = require("../Data.Unit");
var monadEff = new Control_Monad.Monad(function () {
    return applicativeEff;
}, function () {
    return bindEff;
});
var bindEff = new Control_Bind.Bind(function () {
    return applyEff;
}, $foreign.bindE);
var applyEff = new Control_Apply.Apply(function () {
    return functorEff;
}, Control_Monad.ap(monadEff));
var applicativeEff = new Control_Applicative.Applicative(function () {
    return applyEff;
}, $foreign.pureE);
var functorEff = new Data_Functor.Functor(Control_Applicative.liftA1(applicativeEff));
module.exports = {
    functorEff: functorEff, 
    applyEff: applyEff, 
    applicativeEff: applicativeEff, 
    bindEff: bindEff, 
    monadEff: monadEff, 
    forE: $foreign.forE, 
    foreachE: $foreign.foreachE, 
    runPure: $foreign.runPure, 
    untilE: $foreign.untilE, 
    whileE: $foreign.whileE
};

},{"../Control.Applicative":7,"../Control.Apply":9,"../Control.Bind":13,"../Control.Monad":48,"../Data.Functor":100,"../Data.Unit":151,"./foreign":33}],35:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Data_Maybe = require("../Data.Maybe");
var Data_Either = require("../Data.Either");
var Data_Function = require("../Data.Function");
var Data_Unit = require("../Data.Unit");
var MonadError = function (__superclass_Control$dotMonad$dotMonad_0, catchError, throwError) {
    this["__superclass_Control.Monad.Monad_0"] = __superclass_Control$dotMonad$dotMonad_0;
    this.catchError = catchError;
    this.throwError = throwError;
};
var throwError = function (dict) {
    return dict.throwError;
};
var monadErrorMaybe = new MonadError(function () {
    return Data_Maybe.monadMaybe;
}, function (v) {
    return function (v1) {
        if (v instanceof Data_Maybe.Nothing) {
            return v1(Data_Unit.unit);
        };
        if (v instanceof Data_Maybe.Just) {
            return new Data_Maybe.Just(v.value0);
        };
        throw new Error("Failed pattern match at Control.Monad.Error.Class line 55, column 3 - line 55, column 33: " + [ v.constructor.name, v1.constructor.name ]);
    };
}, Data_Function["const"](Data_Maybe.Nothing.value));
var monadErrorEither = new MonadError(function () {
    return Data_Either.monadEither;
}, function (v) {
    return function (v1) {
        if (v instanceof Data_Either.Left) {
            return v1(v.value0);
        };
        if (v instanceof Data_Either.Right) {
            return new Data_Either.Right(v.value0);
        };
        throw new Error("Failed pattern match at Control.Monad.Error.Class line 50, column 3 - line 50, column 30: " + [ v.constructor.name, v1.constructor.name ]);
    };
}, Data_Either.Left.create);
var catchError = function (dict) {
    return dict.catchError;
};
var catchJust = function (dictMonadError) {
    return function (p) {
        return function (act) {
            return function (handler) {
                var handle = function (e) {
                    var $12 = p(e);
                    if ($12 instanceof Data_Maybe.Nothing) {
                        return throwError(dictMonadError)(e);
                    };
                    if ($12 instanceof Data_Maybe.Just) {
                        return handler($12.value0);
                    };
                    throw new Error("Failed pattern match at Control.Monad.Error.Class line 44, column 5 - line 46, column 26: " + [ $12.constructor.name ]);
                };
                return catchError(dictMonadError)(act)(handle);
            };
        };
    };
};
module.exports = {
    MonadError: MonadError, 
    catchError: catchError, 
    catchJust: catchJust, 
    throwError: throwError, 
    monadErrorEither: monadErrorEither, 
    monadErrorMaybe: monadErrorMaybe
};

},{"../Data.Either":77,"../Data.Function":97,"../Data.Maybe":113,"../Data.Unit":151,"../Prelude":162}],36:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Monad_Cont_Class = require("../Control.Monad.Cont.Class");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_Monad_RWS_Class = require("../Control.Monad.RWS.Class");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Control_Monad_Trans = require("../Control.Monad.Trans");
var Control_Monad_Writer_Class = require("../Control.Monad.Writer.Class");
var Control_MonadPlus = require("../Control.MonadPlus");
var Control_MonadZero = require("../Control.MonadZero");
var Control_Plus = require("../Control.Plus");
var Data_Either = require("../Data.Either");
var Data_Monoid = require("../Data.Monoid");
var Data_Tuple = require("../Data.Tuple");
var Data_Functor = require("../Data.Functor");
var Control_Apply = require("../Control.Apply");
var Control_Applicative = require("../Control.Applicative");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Control_Bind = require("../Control.Bind");
var Control_Monad = require("../Control.Monad");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Function = require("../Data.Function");
var Control_Category = require("../Control.Category");
var ExceptT = function (x) {
    return x;
};
var withExceptT = function (dictFunctor) {
    return function (f) {
        return function (v) {
            var mapLeft = function (v1) {
                return function (v2) {
                    if (v2 instanceof Data_Either.Right) {
                        return new Data_Either.Right(v2.value0);
                    };
                    if (v2 instanceof Data_Either.Left) {
                        return new Data_Either.Left(v1(v2.value0));
                    };
                    throw new Error("Failed pattern match at Control.Monad.Except.Trans line 44, column 3 - line 44, column 32: " + [ v1.constructor.name, v2.constructor.name ]);
                };
            };
            return Data_Function.apply(ExceptT)(Data_Functor.map(dictFunctor)(mapLeft(f))(v));
        };
    };
};
var runExceptT = function (v) {
    return v;
};
var monadTransExceptT = new Control_Monad_Trans.MonadTrans(function (dictMonad) {
    return function (m) {
        return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(m)(function (v) {
            return Data_Function.apply(Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]()))(new Data_Either.Right(v));
        });
    };
});
var mapExceptT = function (f) {
    return function (v) {
        return f(v);
    };
};
var functorExceptT = function (dictFunctor) {
    return new Data_Functor.Functor(function (f) {
        return mapExceptT(Data_Functor.map(dictFunctor)(Data_Functor.map(Data_Either.functorEither)(f)));
    });
};
var except = function (dictApplicative) {
    return function ($87) {
        return ExceptT(Control_Applicative.pure(dictApplicative)($87));
    };
};
var applyExceptT = function (dictApply) {
    return new Control_Apply.Apply(function () {
        return functorExceptT(dictApply["__superclass_Data.Functor.Functor_0"]());
    }, function (v) {
        return function (v1) {
            var f$prime = Data_Functor.map(dictApply["__superclass_Data.Functor.Functor_0"]())(Control_Apply.apply(Data_Either.applyEither))(v);
            var x$prime = Control_Apply.apply(dictApply)(f$prime)(v1);
            return x$prime;
        };
    });
};
var bindExceptT = function (dictMonad) {
    return new Control_Bind.Bind(function () {
        return applyExceptT((dictMonad["__superclass_Control.Bind.Bind_1"]())["__superclass_Control.Apply.Apply_0"]());
    }, function (v) {
        return function (k) {
            return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(v)(Data_Either.either(function ($88) {
                return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())(Data_Either.Left.create($88));
            })(function (a) {
                var $56 = k(a);
                return $56;
            }));
        };
    });
};
var applicativeExceptT = function (dictApplicative) {
    return new Control_Applicative.Applicative(function () {
        return applyExceptT(dictApplicative["__superclass_Control.Apply.Apply_0"]());
    }, function ($89) {
        return ExceptT(Control_Applicative.pure(dictApplicative)(Data_Either.Right.create($89)));
    });
};
var monadExceptT = function (dictMonad) {
    return new Control_Monad.Monad(function () {
        return applicativeExceptT(dictMonad["__superclass_Control.Applicative.Applicative_0"]());
    }, function () {
        return bindExceptT(dictMonad);
    });
};
var monadContExceptT = function (dictMonadCont) {
    return new Control_Monad_Cont_Class.MonadCont(function () {
        return monadExceptT(dictMonadCont["__superclass_Control.Monad.Monad_0"]());
    }, function (f) {
        return Data_Function.apply(ExceptT)(Control_Monad_Cont_Class.callCC(dictMonadCont)(function (c) {
            var $57 = f(function (a) {
                return Data_Function.apply(ExceptT)(c(new Data_Either.Right(a)));
            });
            return $57;
        }));
    });
};
var monadEffExceptT = function (dictMonadEff) {
    return new Control_Monad_Eff_Class.MonadEff(function () {
        return monadExceptT(dictMonadEff["__superclass_Control.Monad.Monad_0"]());
    }, function ($90) {
        return Control_Monad_Trans.lift(monadTransExceptT)(dictMonadEff["__superclass_Control.Monad.Monad_0"]())(Control_Monad_Eff_Class.liftEff(dictMonadEff)($90));
    });
};
var monadErrorExceptT = function (dictMonad) {
    return new Control_Monad_Error_Class.MonadError(function () {
        return monadExceptT(dictMonad);
    }, function (v) {
        return function (k) {
            return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(v)(Data_Either.either(function (a) {
                var $60 = k(a);
                return $60;
            })(function ($91) {
                return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())(Data_Either.Right.create($91));
            }));
        };
    }, function ($92) {
        return ExceptT(Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())(Data_Either.Left.create($92)));
    });
};
var monadReaderExceptT = function (dictMonadReader) {
    return new Control_Monad_Reader_Class.MonadReader(function () {
        return monadExceptT(dictMonadReader["__superclass_Control.Monad.Monad_0"]());
    }, Control_Monad_Trans.lift(monadTransExceptT)(dictMonadReader["__superclass_Control.Monad.Monad_0"]())(Control_Monad_Reader_Class.ask(dictMonadReader)), function (f) {
        return mapExceptT(Control_Monad_Reader_Class.local(dictMonadReader)(f));
    });
};
var monadRecExceptT = function (dictMonadRec) {
    return new Control_Monad_Rec_Class.MonadRec(function () {
        return monadExceptT(dictMonadRec["__superclass_Control.Monad.Monad_0"]());
    }, function (f) {
        return function ($93) {
            return ExceptT(Control_Monad_Rec_Class.tailRecM(dictMonadRec)(function (a) {
                return Control_Bind.bind((dictMonadRec["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Bind.Bind_1"]())((function () {
                    var $61 = f(a);
                    return $61;
                })())(function (m$prime) {
                    return Control_Applicative.pure((dictMonadRec["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Applicative.Applicative_0"]())((function () {
                        if (m$prime instanceof Data_Either.Left) {
                            return new Data_Either.Right(new Data_Either.Left(m$prime.value0));
                        };
                        if (m$prime instanceof Data_Either.Right && m$prime.value0 instanceof Data_Either.Left) {
                            return new Data_Either.Left(m$prime.value0.value0);
                        };
                        if (m$prime instanceof Data_Either.Right && m$prime.value0 instanceof Data_Either.Right) {
                            return new Data_Either.Right(new Data_Either.Right(m$prime.value0.value0));
                        };
                        throw new Error("Failed pattern match at Control.Monad.Except.Trans line 77, column 9 - line 80, column 45: " + [ m$prime.constructor.name ]);
                    })());
                });
            })($93));
        };
    });
};
var monadStateExceptT = function (dictMonadState) {
    return new Control_Monad_State_Class.MonadState(function () {
        return monadExceptT(dictMonadState["__superclass_Control.Monad.Monad_0"]());
    }, function (f) {
        return Control_Monad_Trans.lift(monadTransExceptT)(dictMonadState["__superclass_Control.Monad.Monad_0"]())(Control_Monad_State_Class.state(dictMonadState)(f));
    });
};
var monadWriterExceptT = function (dictMonadWriter) {
    return new Control_Monad_Writer_Class.MonadWriter(function () {
        return monadExceptT(dictMonadWriter["__superclass_Control.Monad.Monad_0"]());
    }, mapExceptT(function (m) {
        return Control_Bind.bind((dictMonadWriter["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Bind.Bind_1"]())(Control_Monad_Writer_Class.listen(dictMonadWriter)(m))(function (v) {
            return Data_Function.apply(Control_Applicative.pure((dictMonadWriter["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Applicative.Applicative_0"]()))(Data_Functor.map(Data_Either.functorEither)(function (r) {
                return new Data_Tuple.Tuple(r, v.value1);
            })(v.value0));
        });
    }), mapExceptT(function (m) {
        return Control_Monad_Writer_Class.pass(dictMonadWriter)(Control_Bind.bind((dictMonadWriter["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Bind.Bind_1"]())(m)(function (v) {
            return Control_Applicative.pure((dictMonadWriter["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Applicative.Applicative_0"]())((function () {
                if (v instanceof Data_Either.Left) {
                    return new Data_Tuple.Tuple(new Data_Either.Left(v.value0), Control_Category.id(Control_Category.categoryFn));
                };
                if (v instanceof Data_Either.Right) {
                    return new Data_Tuple.Tuple(new Data_Either.Right(v.value0.value0), v.value0.value1);
                };
                throw new Error("Failed pattern match at Control.Monad.Except.Trans line 133, column 5 - line 135, column 45: " + [ v.constructor.name ]);
            })());
        }));
    }), function (wd) {
        return Control_Monad_Trans.lift(monadTransExceptT)(dictMonadWriter["__superclass_Control.Monad.Monad_0"]())(Control_Monad_Writer_Class.writer(dictMonadWriter)(wd));
    });
};
var monadRWSExceptT = function (dictMonadRWS) {
    return new Control_Monad_RWS_Class.MonadRWS(function () {
        return monadReaderExceptT(dictMonadRWS["__superclass_Control.Monad.Reader.Class.MonadReader_0"]());
    }, function () {
        return monadStateExceptT(dictMonadRWS["__superclass_Control.Monad.State.Class.MonadState_2"]());
    }, function () {
        return monadWriterExceptT(dictMonadRWS["__superclass_Control.Monad.Writer.Class.MonadWriter_1"]());
    });
};
var altExceptT = function (dictSemigroup) {
    return function (dictMonad) {
        return new Control_Alt.Alt(function () {
            return functorExceptT(((dictMonad["__superclass_Control.Bind.Bind_1"]())["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]());
        }, function (v) {
            return function (v1) {
                return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(v)(function (v2) {
                    if (v2 instanceof Data_Either.Right) {
                        return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())(new Data_Either.Right(v2.value0));
                    };
                    if (v2 instanceof Data_Either.Left) {
                        return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(v1)(function (v3) {
                            if (v3 instanceof Data_Either.Right) {
                                return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())(new Data_Either.Right(v3.value0));
                            };
                            if (v3 instanceof Data_Either.Left) {
                                return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())(new Data_Either.Left(Data_Semigroup.append(dictSemigroup)(v2.value0)(v3.value0)));
                            };
                            throw new Error("Failed pattern match at Control.Monad.Except.Trans line 89, column 9 - line 91, column 49: " + [ v3.constructor.name ]);
                        });
                    };
                    throw new Error("Failed pattern match at Control.Monad.Except.Trans line 85, column 5 - line 91, column 49: " + [ v2.constructor.name ]);
                });
            };
        });
    };
};
var plusExceptT = function (dictMonoid) {
    return function (dictMonad) {
        return new Control_Plus.Plus(function () {
            return altExceptT(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]())(dictMonad);
        }, Control_Monad_Error_Class.throwError(monadErrorExceptT(dictMonad))(Data_Monoid.mempty(dictMonoid)));
    };
};
var alternativeExceptT = function (dictMonoid) {
    return function (dictMonad) {
        return new Control_Alternative.Alternative(function () {
            return applicativeExceptT(dictMonad["__superclass_Control.Applicative.Applicative_0"]());
        }, function () {
            return plusExceptT(dictMonoid)(dictMonad);
        });
    };
};
var monadZeroExceptT = function (dictMonoid) {
    return function (dictMonad) {
        return new Control_MonadZero.MonadZero(function () {
            return alternativeExceptT(dictMonoid)(dictMonad);
        }, function () {
            return monadExceptT(dictMonad);
        });
    };
};
var monadPlusExceptT = function (dictMonoid) {
    return function (dictMonad) {
        return new Control_MonadPlus.MonadPlus(function () {
            return monadZeroExceptT(dictMonoid)(dictMonad);
        });
    };
};
module.exports = {
    ExceptT: ExceptT, 
    except: except, 
    mapExceptT: mapExceptT, 
    runExceptT: runExceptT, 
    withExceptT: withExceptT, 
    functorExceptT: functorExceptT, 
    applyExceptT: applyExceptT, 
    applicativeExceptT: applicativeExceptT, 
    bindExceptT: bindExceptT, 
    monadExceptT: monadExceptT, 
    monadRecExceptT: monadRecExceptT, 
    altExceptT: altExceptT, 
    plusExceptT: plusExceptT, 
    alternativeExceptT: alternativeExceptT, 
    monadPlusExceptT: monadPlusExceptT, 
    monadZeroExceptT: monadZeroExceptT, 
    monadTransExceptT: monadTransExceptT, 
    monadEffExceptT: monadEffExceptT, 
    monadContExceptT: monadContExceptT, 
    monadErrorExceptT: monadErrorExceptT, 
    monadReaderExceptT: monadReaderExceptT, 
    monadStateExceptT: monadStateExceptT, 
    monadWriterExceptT: monadWriterExceptT, 
    monadRWSExceptT: monadRWSExceptT
};

},{"../Control.Alt":5,"../Control.Alternative":6,"../Control.Applicative":7,"../Control.Apply":9,"../Control.Bind":13,"../Control.Category":14,"../Control.Monad":48,"../Control.Monad.Cont.Class":22,"../Control.Monad.Eff.Class":24,"../Control.Monad.Error.Class":35,"../Control.Monad.RWS.Class":38,"../Control.Monad.Reader.Class":39,"../Control.Monad.Rec.Class":41,"../Control.Monad.State.Class":44,"../Control.Monad.Trans":45,"../Control.Monad.Writer.Class":46,"../Control.MonadPlus":49,"../Control.MonadZero":50,"../Control.Plus":52,"../Control.Semigroupoid":53,"../Data.Either":77,"../Data.Function":97,"../Data.Functor":100,"../Data.Monoid":120,"../Data.Semigroup":132,"../Data.Tuple":147,"../Prelude":162}],37:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Monad_Cont_Class = require("../Control.Monad.Cont.Class");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_Monad_RWS_Class = require("../Control.Monad.RWS.Class");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Control_Monad_Trans = require("../Control.Monad.Trans");
var Control_Monad_Writer_Class = require("../Control.Monad.Writer.Class");
var Control_MonadPlus = require("../Control.MonadPlus");
var Control_MonadZero = require("../Control.MonadZero");
var Control_Plus = require("../Control.Plus");
var Data_Either = require("../Data.Either");
var Data_Maybe = require("../Data.Maybe");
var Data_Tuple = require("../Data.Tuple");
var Data_Functor = require("../Data.Functor");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Monad = require("../Control.Monad");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Control_Bind = require("../Control.Bind");
var Data_Function = require("../Data.Function");
var Control_Category = require("../Control.Category");
var MaybeT = function (x) {
    return x;
};
var runMaybeT = function (v) {
    return v;
};
var monadTransMaybeT = new Control_Monad_Trans.MonadTrans(function (dictMonad) {
    return function ($60) {
        return MaybeT(Control_Monad.liftM1(dictMonad)(Data_Maybe.Just.create)($60));
    };
});
var mapMaybeT = function (f) {
    return function (v) {
        return f(v);
    };
};
var monadMaybeT = function (dictMonad) {
    return new Control_Monad.Monad(function () {
        return applicativeMaybeT(dictMonad);
    }, function () {
        return bindMaybeT(dictMonad);
    });
};
var functorMaybeT = function (dictMonad) {
    return new Data_Functor.Functor(Control_Applicative.liftA1(applicativeMaybeT(dictMonad)));
};
var bindMaybeT = function (dictMonad) {
    return new Control_Bind.Bind(function () {
        return applyMaybeT(dictMonad);
    }, function (v) {
        return function (f) {
            return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(v)(function (v1) {
                if (v1 instanceof Data_Maybe.Nothing) {
                    return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())(Data_Maybe.Nothing.value);
                };
                if (v1 instanceof Data_Maybe.Just) {
                    var $36 = f(v1.value0);
                    return $36;
                };
                throw new Error("Failed pattern match at Control.Monad.Maybe.Trans line 55, column 5 - line 58, column 22: " + [ v1.constructor.name ]);
            });
        };
    });
};
var applyMaybeT = function (dictMonad) {
    return new Control_Apply.Apply(function () {
        return functorMaybeT(dictMonad);
    }, Control_Monad.ap(monadMaybeT(dictMonad)));
};
var applicativeMaybeT = function (dictMonad) {
    return new Control_Applicative.Applicative(function () {
        return applyMaybeT(dictMonad);
    }, function ($61) {
        return MaybeT(Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())(Data_Maybe.Just.create($61)));
    });
};
var monadContMaybeT = function (dictMonadCont) {
    return new Control_Monad_Cont_Class.MonadCont(function () {
        return monadMaybeT(dictMonadCont["__superclass_Control.Monad.Monad_0"]());
    }, function (f) {
        return Data_Function.apply(MaybeT)(Control_Monad_Cont_Class.callCC(dictMonadCont)(function (c) {
            var $38 = f(function (a) {
                return Data_Function.apply(MaybeT)(Data_Function.apply(c)(new Data_Maybe.Just(a)));
            });
            return $38;
        }));
    });
};
var monadEffMaybe = function (dictMonadEff) {
    return new Control_Monad_Eff_Class.MonadEff(function () {
        return monadMaybeT(dictMonadEff["__superclass_Control.Monad.Monad_0"]());
    }, function ($62) {
        return Control_Monad_Trans.lift(monadTransMaybeT)(dictMonadEff["__superclass_Control.Monad.Monad_0"]())(Control_Monad_Eff_Class.liftEff(dictMonadEff)($62));
    });
};
var monadErrorMaybeT = function (dictMonadError) {
    return new Control_Monad_Error_Class.MonadError(function () {
        return monadMaybeT(dictMonadError["__superclass_Control.Monad.Monad_0"]());
    }, function (v) {
        return function (h) {
            return Data_Function.apply(MaybeT)(Control_Monad_Error_Class.catchError(dictMonadError)(v)(function (a) {
                var $41 = h(a);
                return $41;
            }));
        };
    }, function (e) {
        return Control_Monad_Trans.lift(monadTransMaybeT)(dictMonadError["__superclass_Control.Monad.Monad_0"]())(Control_Monad_Error_Class.throwError(dictMonadError)(e));
    });
};
var monadReaderMaybeT = function (dictMonadReader) {
    return new Control_Monad_Reader_Class.MonadReader(function () {
        return monadMaybeT(dictMonadReader["__superclass_Control.Monad.Monad_0"]());
    }, Control_Monad_Trans.lift(monadTransMaybeT)(dictMonadReader["__superclass_Control.Monad.Monad_0"]())(Control_Monad_Reader_Class.ask(dictMonadReader)), function (f) {
        return mapMaybeT(Control_Monad_Reader_Class.local(dictMonadReader)(f));
    });
};
var monadRecMaybeT = function (dictMonadRec) {
    return new Control_Monad_Rec_Class.MonadRec(function () {
        return monadMaybeT(dictMonadRec["__superclass_Control.Monad.Monad_0"]());
    }, function (f) {
        return function ($63) {
            return MaybeT(Control_Monad_Rec_Class.tailRecM(dictMonadRec)(function (a) {
                var $42 = f(a);
                return Control_Bind.bind((dictMonadRec["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Bind.Bind_1"]())($42)(function (m$prime) {
                    return Control_Applicative.pure((dictMonadRec["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Applicative.Applicative_0"]())((function () {
                        if (m$prime instanceof Data_Maybe.Nothing) {
                            return new Data_Either.Right(Data_Maybe.Nothing.value);
                        };
                        if (m$prime instanceof Data_Maybe.Just && m$prime.value0 instanceof Data_Either.Left) {
                            return new Data_Either.Left(m$prime.value0.value0);
                        };
                        if (m$prime instanceof Data_Maybe.Just && m$prime.value0 instanceof Data_Either.Right) {
                            return new Data_Either.Right(new Data_Maybe.Just(m$prime.value0.value0));
                        };
                        throw new Error("Failed pattern match at Control.Monad.Maybe.Trans line 86, column 11 - line 89, column 45: " + [ m$prime.constructor.name ]);
                    })());
                });
            })($63));
        };
    });
};
var monadStateMaybeT = function (dictMonadState) {
    return new Control_Monad_State_Class.MonadState(function () {
        return monadMaybeT(dictMonadState["__superclass_Control.Monad.Monad_0"]());
    }, function (f) {
        return Control_Monad_Trans.lift(monadTransMaybeT)(dictMonadState["__superclass_Control.Monad.Monad_0"]())(Control_Monad_State_Class.state(dictMonadState)(f));
    });
};
var monadWriterMaybeT = function (dictMonadWriter) {
    return new Control_Monad_Writer_Class.MonadWriter(function () {
        return monadMaybeT(dictMonadWriter["__superclass_Control.Monad.Monad_0"]());
    }, mapMaybeT(function (m) {
        return Control_Bind.bind((dictMonadWriter["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Bind.Bind_1"]())(Control_Monad_Writer_Class.listen(dictMonadWriter)(m))(function (v) {
            return Data_Function.apply(Control_Applicative.pure((dictMonadWriter["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Applicative.Applicative_0"]()))(Data_Functor.map(Data_Maybe.functorMaybe)(function (r) {
                return new Data_Tuple.Tuple(r, v.value1);
            })(v.value0));
        });
    }), mapMaybeT(function (m) {
        return Control_Monad_Writer_Class.pass(dictMonadWriter)(Control_Bind.bind((dictMonadWriter["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Bind.Bind_1"]())(m)(function (v) {
            return Control_Applicative.pure((dictMonadWriter["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Applicative.Applicative_0"]())((function () {
                if (v instanceof Data_Maybe.Nothing) {
                    return new Data_Tuple.Tuple(Data_Maybe.Nothing.value, Control_Category.id(Control_Category.categoryFn));
                };
                if (v instanceof Data_Maybe.Just) {
                    return new Data_Tuple.Tuple(new Data_Maybe.Just(v.value0.value0), v.value0.value1);
                };
                throw new Error("Failed pattern match at Control.Monad.Maybe.Trans line 117, column 5 - line 119, column 43: " + [ v.constructor.name ]);
            })());
        }));
    }), function (wd) {
        return Control_Monad_Trans.lift(monadTransMaybeT)(dictMonadWriter["__superclass_Control.Monad.Monad_0"]())(Control_Monad_Writer_Class.writer(dictMonadWriter)(wd));
    });
};
var monadRWSMaybeT = function (dictMonadRWS) {
    return new Control_Monad_RWS_Class.MonadRWS(function () {
        return monadReaderMaybeT(dictMonadRWS["__superclass_Control.Monad.Reader.Class.MonadReader_0"]());
    }, function () {
        return monadStateMaybeT(dictMonadRWS["__superclass_Control.Monad.State.Class.MonadState_2"]());
    }, function () {
        return monadWriterMaybeT(dictMonadRWS["__superclass_Control.Monad.Writer.Class.MonadWriter_1"]());
    });
};
var altMaybeT = function (dictMonad) {
    return new Control_Alt.Alt(function () {
        return functorMaybeT(dictMonad);
    }, function (v) {
        return function (v1) {
            return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(v)(function (v2) {
                if (v2 instanceof Data_Maybe.Nothing) {
                    return v1;
                };
                return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())(v2);
            });
        };
    });
};
var plusMaybeT = function (dictMonad) {
    return new Control_Plus.Plus(function () {
        return altMaybeT(dictMonad);
    }, Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())(Data_Maybe.Nothing.value));
};
var alternativeMaybeT = function (dictMonad) {
    return new Control_Alternative.Alternative(function () {
        return applicativeMaybeT(dictMonad);
    }, function () {
        return plusMaybeT(dictMonad);
    });
};
var monadZeroMaybeT = function (dictMonad) {
    return new Control_MonadZero.MonadZero(function () {
        return alternativeMaybeT(dictMonad);
    }, function () {
        return monadMaybeT(dictMonad);
    });
};
var monadPlusMaybeT = function (dictMonad) {
    return new Control_MonadPlus.MonadPlus(function () {
        return monadZeroMaybeT(dictMonad);
    });
};
module.exports = {
    MaybeT: MaybeT, 
    mapMaybeT: mapMaybeT, 
    runMaybeT: runMaybeT, 
    functorMaybeT: functorMaybeT, 
    applyMaybeT: applyMaybeT, 
    applicativeMaybeT: applicativeMaybeT, 
    bindMaybeT: bindMaybeT, 
    monadMaybeT: monadMaybeT, 
    monadTransMaybeT: monadTransMaybeT, 
    altMaybeT: altMaybeT, 
    plusMaybeT: plusMaybeT, 
    alternativeMaybeT: alternativeMaybeT, 
    monadPlusMaybeT: monadPlusMaybeT, 
    monadZeroMaybeT: monadZeroMaybeT, 
    monadRecMaybeT: monadRecMaybeT, 
    monadEffMaybe: monadEffMaybe, 
    monadContMaybeT: monadContMaybeT, 
    monadErrorMaybeT: monadErrorMaybeT, 
    monadReaderMaybeT: monadReaderMaybeT, 
    monadStateMaybeT: monadStateMaybeT, 
    monadWriterMaybeT: monadWriterMaybeT, 
    monadRWSMaybeT: monadRWSMaybeT
};

},{"../Control.Alt":5,"../Control.Alternative":6,"../Control.Applicative":7,"../Control.Apply":9,"../Control.Bind":13,"../Control.Category":14,"../Control.Monad":48,"../Control.Monad.Cont.Class":22,"../Control.Monad.Eff.Class":24,"../Control.Monad.Error.Class":35,"../Control.Monad.RWS.Class":38,"../Control.Monad.Reader.Class":39,"../Control.Monad.Rec.Class":41,"../Control.Monad.State.Class":44,"../Control.Monad.Trans":45,"../Control.Monad.Writer.Class":46,"../Control.MonadPlus":49,"../Control.MonadZero":50,"../Control.Plus":52,"../Control.Semigroupoid":53,"../Data.Either":77,"../Data.Function":97,"../Data.Functor":100,"../Data.Maybe":113,"../Data.Tuple":147,"../Prelude":162}],38:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Control_Monad_Trans = require("../Control.Monad.Trans");
var Control_Monad_Writer_Class = require("../Control.Monad.Writer.Class");
var MonadRWS = function (__superclass_Control$dotMonad$dotReader$dotClass$dotMonadReader_0, __superclass_Control$dotMonad$dotState$dotClass$dotMonadState_2, __superclass_Control$dotMonad$dotWriter$dotClass$dotMonadWriter_1) {
    this["__superclass_Control.Monad.Reader.Class.MonadReader_0"] = __superclass_Control$dotMonad$dotReader$dotClass$dotMonadReader_0;
    this["__superclass_Control.Monad.State.Class.MonadState_2"] = __superclass_Control$dotMonad$dotState$dotClass$dotMonadState_2;
    this["__superclass_Control.Monad.Writer.Class.MonadWriter_1"] = __superclass_Control$dotMonad$dotWriter$dotClass$dotMonadWriter_1;
};
module.exports = {
    MonadRWS: MonadRWS
};

},{"../Control.Monad.Reader.Class":39,"../Control.Monad.State.Class":44,"../Control.Monad.Trans":45,"../Control.Monad.Writer.Class":46}],39:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Control_Category = require("../Control.Category");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Control_Monad = require("../Control.Monad");
var Control_Bind = require("../Control.Bind");
var Control_Applicative = require("../Control.Applicative");
var MonadReader = function (__superclass_Control$dotMonad$dotMonad_0, ask, local) {
    this["__superclass_Control.Monad.Monad_0"] = __superclass_Control$dotMonad$dotMonad_0;
    this.ask = ask;
    this.local = local;
};
var monadReaderFun = new MonadReader(function () {
    return Control_Monad.monadFn;
}, Control_Category.id(Control_Category.categoryFn), Control_Semigroupoid.composeFlipped(Control_Semigroupoid.semigroupoidFn));
var local = function (dict) {
    return dict.local;
};
var ask = function (dict) {
    return dict.ask;
};
var reader = function (dictMonadReader) {
    return function (f) {
        return Control_Bind.bindFlipped((dictMonadReader["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Bind.Bind_1"]())(function ($1) {
            return Control_Applicative.pure((dictMonadReader["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Applicative.Applicative_0"]())(f($1));
        })(ask(dictMonadReader));
    };
};
module.exports = {
    MonadReader: MonadReader, 
    ask: ask, 
    local: local, 
    reader: reader, 
    monadReaderFun: monadReaderFun
};

},{"../Control.Applicative":7,"../Control.Bind":13,"../Control.Category":14,"../Control.Monad":48,"../Control.Semigroupoid":53,"../Prelude":162}],40:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Monad_Cont_Class = require("../Control.Monad.Cont.Class");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Control_Monad_Trans = require("../Control.Monad.Trans");
var Control_Monad_Writer_Class = require("../Control.Monad.Writer.Class");
var Control_MonadPlus = require("../Control.MonadPlus");
var Control_MonadZero = require("../Control.MonadZero");
var Control_Plus = require("../Control.Plus");
var Data_Distributive = require("../Data.Distributive");
var Data_Either = require("../Data.Either");
var Data_Functor = require("../Data.Functor");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Control_Apply = require("../Control.Apply");
var Control_Applicative = require("../Control.Applicative");
var Data_Function = require("../Data.Function");
var Control_Bind = require("../Control.Bind");
var Control_Monad = require("../Control.Monad");
var ReaderT = function (x) {
    return x;
};
var withReaderT = function (f) {
    return function (v) {
        return function ($48) {
            return v(f($48));
        };
    };
};
var runReaderT = function (v) {
    return v;
};
var monadTransReaderT = new Control_Monad_Trans.MonadTrans(function (dictMonad) {
    return function ($49) {
        return ReaderT(Data_Function["const"]($49));
    };
});
var mapReaderT = function (f) {
    return function (v) {
        return function ($50) {
            return f(v($50));
        };
    };
};
var functorReaderT = function (dictFunctor) {
    return new Data_Functor.Functor(function ($51) {
        return mapReaderT(Data_Functor.map(dictFunctor)($51));
    });
};
var distributiveReaderT = function (dictDistributive) {
    return new Data_Distributive.Distributive(function () {
        return functorReaderT(dictDistributive["__superclass_Data.Functor.Functor_0"]());
    }, function (dictFunctor) {
        return function (f) {
            return function ($52) {
                return Data_Distributive.distribute(distributiveReaderT(dictDistributive))(dictFunctor)(Data_Functor.map(dictFunctor)(f)($52));
            };
        };
    }, function (dictFunctor) {
        return function (a) {
            return function (e) {
                return Data_Distributive.collect(dictDistributive)(dictFunctor)(function (r) {
                    return r(e);
                })(a);
            };
        };
    });
};
var applyReaderT = function (dictApply) {
    return new Control_Apply.Apply(function () {
        return functorReaderT(dictApply["__superclass_Data.Functor.Functor_0"]());
    }, function (v) {
        return function (v1) {
            return function (r) {
                return Control_Apply.apply(dictApply)(v(r))(v1(r));
            };
        };
    });
};
var bindReaderT = function (dictBind) {
    return new Control_Bind.Bind(function () {
        return applyReaderT(dictBind["__superclass_Control.Apply.Apply_0"]());
    }, function (v) {
        return function (k) {
            return function (r) {
                return Control_Bind.bind(dictBind)(v(r))(function (a) {
                    var $40 = k(a);
                    return $40(r);
                });
            };
        };
    });
};
var applicativeReaderT = function (dictApplicative) {
    return new Control_Applicative.Applicative(function () {
        return applyReaderT(dictApplicative["__superclass_Control.Apply.Apply_0"]());
    }, function ($53) {
        return ReaderT(Data_Function["const"](Control_Applicative.pure(dictApplicative)($53)));
    });
};
var monadReaderT = function (dictMonad) {
    return new Control_Monad.Monad(function () {
        return applicativeReaderT(dictMonad["__superclass_Control.Applicative.Applicative_0"]());
    }, function () {
        return bindReaderT(dictMonad["__superclass_Control.Bind.Bind_1"]());
    });
};
var monadContReaderT = function (dictMonadCont) {
    return new Control_Monad_Cont_Class.MonadCont(function () {
        return monadReaderT(dictMonadCont["__superclass_Control.Monad.Monad_0"]());
    }, function (f) {
        return function (r) {
            return Control_Monad_Cont_Class.callCC(dictMonadCont)(function (c) {
                var $41 = f(function ($54) {
                    return ReaderT(Data_Function["const"](c($54)));
                });
                return $41(r);
            });
        };
    });
};
var monadEffReader = function (dictMonadEff) {
    return new Control_Monad_Eff_Class.MonadEff(function () {
        return monadReaderT(dictMonadEff["__superclass_Control.Monad.Monad_0"]());
    }, function ($55) {
        return Control_Monad_Trans.lift(monadTransReaderT)(dictMonadEff["__superclass_Control.Monad.Monad_0"]())(Control_Monad_Eff_Class.liftEff(dictMonadEff)($55));
    });
};
var monadErrorReaderT = function (dictMonadError) {
    return new Control_Monad_Error_Class.MonadError(function () {
        return monadReaderT(dictMonadError["__superclass_Control.Monad.Monad_0"]());
    }, function (v) {
        return function (h) {
            return function (r) {
                return Control_Monad_Error_Class.catchError(dictMonadError)(v(r))(function (e) {
                    var $44 = h(e);
                    return $44(r);
                });
            };
        };
    }, function ($56) {
        return Control_Monad_Trans.lift(monadTransReaderT)(dictMonadError["__superclass_Control.Monad.Monad_0"]())(Control_Monad_Error_Class.throwError(dictMonadError)($56));
    });
};
var monadReaderReaderT = function (dictMonad) {
    return new Control_Monad_Reader_Class.MonadReader(function () {
        return monadReaderT(dictMonad);
    }, Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]()), withReaderT);
};
var monadRecReaderT = function (dictMonadRec) {
    return new Control_Monad_Rec_Class.MonadRec(function () {
        return monadReaderT(dictMonadRec["__superclass_Control.Monad.Monad_0"]());
    }, function (k) {
        return function (a) {
            var k$prime = function (r) {
                return function (a1) {
                    var $45 = k(a1);
                    return Control_Bind.bindFlipped((dictMonadRec["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Bind.Bind_1"]())(function ($57) {
                        return Control_Applicative.pure((dictMonadRec["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Applicative.Applicative_0"]())(Data_Either.either(Data_Either.Left.create)(Data_Either.Right.create)($57));
                    })($45(r));
                };
            };
            return function (r) {
                return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(k$prime(r))(a);
            };
        };
    });
};
var monadStateReaderT = function (dictMonadState) {
    return new Control_Monad_State_Class.MonadState(function () {
        return monadReaderT(dictMonadState["__superclass_Control.Monad.Monad_0"]());
    }, function ($58) {
        return Control_Monad_Trans.lift(monadTransReaderT)(dictMonadState["__superclass_Control.Monad.Monad_0"]())(Control_Monad_State_Class.state(dictMonadState)($58));
    });
};
var monadWriterReaderT = function (dictMonadWriter) {
    return new Control_Monad_Writer_Class.MonadWriter(function () {
        return monadReaderT(dictMonadWriter["__superclass_Control.Monad.Monad_0"]());
    }, mapReaderT(Control_Monad_Writer_Class.listen(dictMonadWriter)), mapReaderT(Control_Monad_Writer_Class.pass(dictMonadWriter)), function ($59) {
        return Control_Monad_Trans.lift(monadTransReaderT)(dictMonadWriter["__superclass_Control.Monad.Monad_0"]())(Control_Monad_Writer_Class.writer(dictMonadWriter)($59));
    });
};
var altReaderT = function (dictAlt) {
    return new Control_Alt.Alt(function () {
        return functorReaderT(dictAlt["__superclass_Data.Functor.Functor_0"]());
    }, function (v) {
        return function (v1) {
            return function (r) {
                return Control_Alt.alt(dictAlt)(v(r))(v1(r));
            };
        };
    });
};
var plusReaderT = function (dictPlus) {
    return new Control_Plus.Plus(function () {
        return altReaderT(dictPlus["__superclass_Control.Alt.Alt_0"]());
    }, Data_Function["const"](Control_Plus.empty(dictPlus)));
};
var alternativeReaderT = function (dictAlternative) {
    return new Control_Alternative.Alternative(function () {
        return applicativeReaderT(dictAlternative["__superclass_Control.Applicative.Applicative_0"]());
    }, function () {
        return plusReaderT(dictAlternative["__superclass_Control.Plus.Plus_1"]());
    });
};
var monadZeroReaderT = function (dictMonadZero) {
    return new Control_MonadZero.MonadZero(function () {
        return alternativeReaderT(dictMonadZero["__superclass_Control.Alternative.Alternative_1"]());
    }, function () {
        return monadReaderT(dictMonadZero["__superclass_Control.Monad.Monad_0"]());
    });
};
var monadPlusReaderT = function (dictMonadPlus) {
    return new Control_MonadPlus.MonadPlus(function () {
        return monadZeroReaderT(dictMonadPlus["__superclass_Control.MonadZero.MonadZero_0"]());
    });
};
module.exports = {
    ReaderT: ReaderT, 
    mapReaderT: mapReaderT, 
    runReaderT: runReaderT, 
    withReaderT: withReaderT, 
    functorReaderT: functorReaderT, 
    applyReaderT: applyReaderT, 
    applicativeReaderT: applicativeReaderT, 
    altReaderT: altReaderT, 
    plusReaderT: plusReaderT, 
    alternativeReaderT: alternativeReaderT, 
    bindReaderT: bindReaderT, 
    monadReaderT: monadReaderT, 
    monadZeroReaderT: monadZeroReaderT, 
    monadPlusReaderT: monadPlusReaderT, 
    monadTransReaderT: monadTransReaderT, 
    monadEffReader: monadEffReader, 
    monadContReaderT: monadContReaderT, 
    monadErrorReaderT: monadErrorReaderT, 
    monadReaderReaderT: monadReaderReaderT, 
    monadStateReaderT: monadStateReaderT, 
    monadWriterReaderT: monadWriterReaderT, 
    distributiveReaderT: distributiveReaderT, 
    monadRecReaderT: monadRecReaderT
};

},{"../Control.Alt":5,"../Control.Alternative":6,"../Control.Applicative":7,"../Control.Apply":9,"../Control.Bind":13,"../Control.Monad":48,"../Control.Monad.Cont.Class":22,"../Control.Monad.Eff.Class":24,"../Control.Monad.Error.Class":35,"../Control.Monad.Reader.Class":39,"../Control.Monad.Rec.Class":41,"../Control.Monad.State.Class":44,"../Control.Monad.Trans":45,"../Control.Monad.Writer.Class":46,"../Control.MonadPlus":49,"../Control.MonadZero":50,"../Control.Plus":52,"../Control.Semigroupoid":53,"../Data.Distributive":76,"../Data.Either":77,"../Data.Function":97,"../Data.Functor":100,"../Prelude":162}],41:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Unsafe = require("../Control.Monad.Eff.Unsafe");
var Control_Monad_ST = require("../Control.Monad.ST");
var Data_Either = require("../Data.Either");
var Data_Identity = require("../Data.Identity");
var Partial_Unsafe = require("../Partial.Unsafe");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Control_Bind = require("../Control.Bind");
var Control_Applicative = require("../Control.Applicative");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Unit = require("../Data.Unit");
var MonadRec = function (__superclass_Control$dotMonad$dotMonad_0, tailRecM) {
    this["__superclass_Control.Monad.Monad_0"] = __superclass_Control$dotMonad$dotMonad_0;
    this.tailRecM = tailRecM;
};
var tailRecM = function (dict) {
    return dict.tailRecM;
};
var tailRecM2 = function (dictMonadRec) {
    return function (f) {
        return function (a) {
            return function (b) {
                return tailRecM(dictMonadRec)(function (o) {
                    return f(o.a)(o.b);
                })({
                    a: a, 
                    b: b
                });
            };
        };
    };
};
var tailRecM3 = function (dictMonadRec) {
    return function (f) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return tailRecM(dictMonadRec)(function (o) {
                        return f(o.a)(o.b)(o.c);
                    })({
                        a: a, 
                        b: b, 
                        c: c
                    });
                };
            };
        };
    };
};
var tailRecEff = function (f) {
    return function (a) {
        var f$prime = function ($26) {
            return Control_Monad_Eff_Unsafe.unsafeInterleaveEff(f($26));
        };
        return function __do() {
            var v = f$prime(a)();
            var v1 = {
                value: v
            };
            (function () {
                while (!(function __do() {
                    var v2 = v1.value;
                    if (v2 instanceof Data_Either.Left) {
                        var v3 = f$prime(v2.value0)();
                        v1.value = v3;
                        return false;
                    };
                    if (v2 instanceof Data_Either.Right) {
                        return true;
                    };
                    throw new Error("Failed pattern match at Control.Monad.Rec.Class line 103, column 5 - line 108, column 27: " + [ v2.constructor.name ]);
                })()) {

                };
                return {};
            })();
            return Data_Function.apply(Partial_Unsafe.unsafePartial)(function (dictPartial) {
                return Data_Functor.map(Control_Monad_Eff.functorEff)(Data_Either.fromRight(dictPartial))(Control_Monad_ST.readSTRef(v1));
            })();
        };
    };
};
var tailRec = function (f) {
    return function (a) {
        var go = function (__copy_v) {
            var v = __copy_v;
            tco: while (true) {
                if (v instanceof Data_Either.Left) {
                    var __tco_v = f(v.value0);
                    v = __tco_v;
                    continue tco;
                };
                if (v instanceof Data_Either.Right) {
                    return v.value0;
                };
                throw new Error("Failed pattern match at Control.Monad.Rec.Class line 78, column 1 - line 81, column 19: " + [ v.constructor.name ]);
            };
        };
        return go(f(a));
    };
};
var monadRecIdentity = new MonadRec(function () {
    return Data_Identity.monadIdentity;
}, function (f) {
    return function ($27) {
        return Data_Identity.Identity(tailRec(function ($28) {
            return Data_Identity.runIdentity(f($28));
        })($27));
    };
});
var monadRecEither = new MonadRec(function () {
    return Data_Either.monadEither;
}, function (f) {
    return function (a0) {
        var g = function (v) {
            if (v instanceof Data_Either.Left) {
                return new Data_Either.Right(new Data_Either.Left(v.value0));
            };
            if (v instanceof Data_Either.Right && v.value0 instanceof Data_Either.Left) {
                return new Data_Either.Left(f(v.value0.value0));
            };
            if (v instanceof Data_Either.Right && v.value0 instanceof Data_Either.Right) {
                return new Data_Either.Right(new Data_Either.Right(v.value0.value0));
            };
            throw new Error("Failed pattern match at Control.Monad.Rec.Class line 92, column 7 - line 92, column 34: " + [ v.constructor.name ]);
        };
        return tailRec(g)(f(a0));
    };
});
var monadRecEff = new MonadRec(function () {
    return Control_Monad_Eff.monadEff;
}, tailRecEff);
var forever = function (dictMonadRec) {
    return function (ma) {
        return tailRecM(dictMonadRec)(function (u) {
            return Data_Functor.voidRight((((dictMonadRec["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Bind.Bind_1"]())["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(new Data_Either.Left(u))(ma);
        })(Data_Unit.unit);
    };
};
module.exports = {
    MonadRec: MonadRec, 
    forever: forever, 
    tailRec: tailRec, 
    tailRecM: tailRecM, 
    tailRecM2: tailRecM2, 
    tailRecM3: tailRecM3, 
    monadRecIdentity: monadRecIdentity, 
    monadRecEff: monadRecEff, 
    monadRecEither: monadRecEither
};

},{"../Control.Applicative":7,"../Control.Bind":13,"../Control.Monad.Eff":34,"../Control.Monad.Eff.Unsafe":32,"../Control.Monad.ST":43,"../Control.Semigroupoid":53,"../Data.Either":77,"../Data.Function":97,"../Data.Functor":100,"../Data.Identity":105,"../Data.Unit":151,"../Partial.Unsafe":159,"../Prelude":162}],42:[function(require,module,exports){
/* global exports */
"use strict";

// module Control.Monad.ST

exports.newSTRef = function (val) {
  return function () {
    return { value: val };
  };
};

exports.readSTRef = function (ref) {
  return function () {
    return ref.value;
  };
};

exports.modifySTRef = function (ref) {
  return function (f) {
    return function () {
      /* jshint boss: true */
      return ref.value = f(ref.value);
    };
  };
};

exports.writeSTRef = function (ref) {
  return function (a) {
    return function () {
      /* jshint boss: true */
      return ref.value = a;
    };
  };
};

exports.runST = function (f) {
  return f;
};

},{}],43:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var pureST = function (st) {
    return Control_Monad_Eff.runPure($foreign.runST(st));
};
module.exports = {
    pureST: pureST, 
    modifySTRef: $foreign.modifySTRef, 
    newSTRef: $foreign.newSTRef, 
    readSTRef: $foreign.readSTRef, 
    runST: $foreign.runST, 
    writeSTRef: $foreign.writeSTRef
};

},{"../Control.Monad.Eff":34,"./foreign":42}],44:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Data_Tuple = require("../Data.Tuple");
var Data_Unit = require("../Data.Unit");
var MonadState = function (__superclass_Control$dotMonad$dotMonad_0, state) {
    this["__superclass_Control.Monad.Monad_0"] = __superclass_Control$dotMonad$dotMonad_0;
    this.state = state;
};
var state = function (dict) {
    return dict.state;
};
var put = function (dictMonadState) {
    return function (s) {
        return state(dictMonadState)(function (v) {
            return new Data_Tuple.Tuple(Data_Unit.unit, s);
        });
    };
};
var modify = function (dictMonadState) {
    return function (f) {
        return state(dictMonadState)(function (s) {
            return new Data_Tuple.Tuple(Data_Unit.unit, f(s));
        });
    };
};
var gets = function (dictMonadState) {
    return function (f) {
        return state(dictMonadState)(function (s) {
            return new Data_Tuple.Tuple(f(s), s);
        });
    };
};
var get = function (dictMonadState) {
    return state(dictMonadState)(function (s) {
        return new Data_Tuple.Tuple(s, s);
    });
};
module.exports = {
    MonadState: MonadState, 
    get: get, 
    gets: gets, 
    modify: modify, 
    put: put, 
    state: state
};

},{"../Data.Tuple":147,"../Data.Unit":151,"../Prelude":162}],45:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var MonadTrans = function (lift) {
    this.lift = lift;
};
var lift = function (dict) {
    return dict.lift;
};
module.exports = {
    MonadTrans: MonadTrans, 
    lift: lift
};

},{"../Prelude":162}],46:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Data_Tuple = require("../Data.Tuple");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Unit = require("../Data.Unit");
var Control_Bind = require("../Control.Bind");
var Data_Function = require("../Data.Function");
var Control_Applicative = require("../Control.Applicative");
var MonadWriter = function (__superclass_Control$dotMonad$dotMonad_0, listen, pass, writer) {
    this["__superclass_Control.Monad.Monad_0"] = __superclass_Control$dotMonad$dotMonad_0;
    this.listen = listen;
    this.pass = pass;
    this.writer = writer;
};
var writer = function (dict) {
    return dict.writer;
};
var tell = function (dictMonadWriter) {
    return function ($9) {
        return writer(dictMonadWriter)(Data_Tuple.Tuple.create(Data_Unit.unit)($9));
    };
};
var pass = function (dict) {
    return dict.pass;
};
var listen = function (dict) {
    return dict.listen;
};
var listens = function (dictMonadWriter) {
    return function (f) {
        return function (m) {
            return Control_Bind.bind((dictMonadWriter["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Bind.Bind_1"]())(listen(dictMonadWriter)(m))(function (v) {
                return Data_Function.apply(Control_Applicative.pure((dictMonadWriter["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Applicative.Applicative_0"]()))(new Data_Tuple.Tuple(v.value0, f(v.value1)));
            });
        };
    };
};
var censor = function (dictMonadWriter) {
    return function (f) {
        return function (m) {
            return pass(dictMonadWriter)(Control_Bind.bind((dictMonadWriter["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Bind.Bind_1"]())(m)(function (v) {
                return Data_Function.apply(Control_Applicative.pure((dictMonadWriter["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Applicative.Applicative_0"]()))(new Data_Tuple.Tuple(v, f));
            }));
        };
    };
};
module.exports = {
    MonadWriter: MonadWriter, 
    censor: censor, 
    listen: listen, 
    listens: listens, 
    pass: pass, 
    tell: tell, 
    writer: writer
};

},{"../Control.Applicative":7,"../Control.Bind":13,"../Control.Semigroupoid":53,"../Data.Function":97,"../Data.Tuple":147,"../Data.Unit":151,"../Prelude":162}],47:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Data_Either = require("../Data.Either");
var Data_Monoid = require("../Data.Monoid");
var Data_Tuple = require("../Data.Tuple");
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Monad_Cont_Class = require("../Control.Monad.Cont.Class");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Control_Monad_Trans = require("../Control.Monad.Trans");
var Control_Monad_Writer_Class = require("../Control.Monad.Writer.Class");
var Control_MonadPlus = require("../Control.MonadPlus");
var Control_MonadZero = require("../Control.MonadZero");
var Control_Plus = require("../Control.Plus");
var Data_Functor = require("../Data.Functor");
var Data_Function = require("../Data.Function");
var Control_Apply = require("../Control.Apply");
var Data_Semigroup = require("../Data.Semigroup");
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad = require("../Control.Monad");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var WriterT = function (x) {
    return x;
};
var runWriterT = function (v) {
    return v;
};
var monadTransWriterT = function (dictMonoid) {
    return new Control_Monad_Trans.MonadTrans(function (dictMonad) {
        return function (m) {
            return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(m)(function (v) {
                return Data_Function.apply(Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]()))(new Data_Tuple.Tuple(v, Data_Monoid.mempty(dictMonoid)));
            });
        };
    });
};
var mapWriterT = function (f) {
    return function (v) {
        return f(v);
    };
};
var functorWriterT = function (dictFunctor) {
    return new Data_Functor.Functor(function (f) {
        return Data_Function.apply(mapWriterT)(Data_Functor.map(dictFunctor)(function (v) {
            return new Data_Tuple.Tuple(f(v.value0), v.value1);
        }));
    });
};
var execWriterT = function (dictFunctor) {
    return function (v) {
        return Data_Functor.map(dictFunctor)(Data_Tuple.snd)(v);
    };
};
var applyWriterT = function (dictSemigroup) {
    return function (dictApply) {
        return new Control_Apply.Apply(function () {
            return functorWriterT(dictApply["__superclass_Data.Functor.Functor_0"]());
        }, function (v) {
            return function (v1) {
                var k = function (v3) {
                    return function (v4) {
                        return new Data_Tuple.Tuple(v3.value0(v4.value0), Data_Semigroup.append(dictSemigroup)(v3.value1)(v4.value1));
                    };
                };
                return Control_Apply.apply(dictApply)(Data_Functor.map(dictApply["__superclass_Data.Functor.Functor_0"]())(k)(v))(v1);
            };
        });
    };
};
var bindWriterT = function (dictSemigroup) {
    return function (dictMonad) {
        return new Control_Bind.Bind(function () {
            return applyWriterT(dictSemigroup)((dictMonad["__superclass_Control.Bind.Bind_1"]())["__superclass_Control.Apply.Apply_0"]());
        }, function (v) {
            return function (k) {
                return Data_Function.apply(WriterT)(Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(v)(function (v1) {
                    return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())((function () {
                        var $74 = k(v1.value0);
                        return $74;
                    })())(function (v2) {
                        return Data_Function.apply(Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]()))(new Data_Tuple.Tuple(v2.value0, Data_Semigroup.append(dictSemigroup)(v1.value1)(v2.value1)));
                    });
                }));
            };
        });
    };
};
var applicativeWriterT = function (dictMonoid) {
    return function (dictApplicative) {
        return new Control_Applicative.Applicative(function () {
            return applyWriterT(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]())(dictApplicative["__superclass_Control.Apply.Apply_0"]());
        }, function (a) {
            return Data_Function.apply(WriterT)(Data_Function.apply(Control_Applicative.pure(dictApplicative))(new Data_Tuple.Tuple(a, Data_Monoid.mempty(dictMonoid))));
        });
    };
};
var monadWriterT = function (dictMonoid) {
    return function (dictMonad) {
        return new Control_Monad.Monad(function () {
            return applicativeWriterT(dictMonoid)(dictMonad["__superclass_Control.Applicative.Applicative_0"]());
        }, function () {
            return bindWriterT(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]())(dictMonad);
        });
    };
};
var monadContWriterT = function (dictMonoid) {
    return function (dictMonadCont) {
        return new Control_Monad_Cont_Class.MonadCont(function () {
            return monadWriterT(dictMonoid)(dictMonadCont["__superclass_Control.Monad.Monad_0"]());
        }, function (f) {
            return Data_Function.apply(WriterT)(Control_Monad_Cont_Class.callCC(dictMonadCont)(function (c) {
                var $80 = f(function (a) {
                    return Data_Function.apply(WriterT)(c(new Data_Tuple.Tuple(a, Data_Monoid.mempty(dictMonoid))));
                });
                return $80;
            }));
        });
    };
};
var monadEffWriter = function (dictMonoid) {
    return function (dictMonadEff) {
        return new Control_Monad_Eff_Class.MonadEff(function () {
            return monadWriterT(dictMonoid)(dictMonadEff["__superclass_Control.Monad.Monad_0"]());
        }, function ($106) {
            return Control_Monad_Trans.lift(monadTransWriterT(dictMonoid))(dictMonadEff["__superclass_Control.Monad.Monad_0"]())(Control_Monad_Eff_Class.liftEff(dictMonadEff)($106));
        });
    };
};
var monadErrorWriterT = function (dictMonoid) {
    return function (dictMonadError) {
        return new Control_Monad_Error_Class.MonadError(function () {
            return monadWriterT(dictMonoid)(dictMonadError["__superclass_Control.Monad.Monad_0"]());
        }, function (v) {
            return function (h) {
                return Data_Function.apply(WriterT)(Control_Monad_Error_Class.catchError(dictMonadError)(v)(function (e) {
                    var $83 = h(e);
                    return $83;
                }));
            };
        }, function (e) {
            return Control_Monad_Trans.lift(monadTransWriterT(dictMonoid))(dictMonadError["__superclass_Control.Monad.Monad_0"]())(Control_Monad_Error_Class.throwError(dictMonadError)(e));
        });
    };
};
var monadReaderWriterT = function (dictMonoid) {
    return function (dictMonadReader) {
        return new Control_Monad_Reader_Class.MonadReader(function () {
            return monadWriterT(dictMonoid)(dictMonadReader["__superclass_Control.Monad.Monad_0"]());
        }, Control_Monad_Trans.lift(monadTransWriterT(dictMonoid))(dictMonadReader["__superclass_Control.Monad.Monad_0"]())(Control_Monad_Reader_Class.ask(dictMonadReader)), function (f) {
            return mapWriterT(Control_Monad_Reader_Class.local(dictMonadReader)(f));
        });
    };
};
var monadRecWriterT = function (dictMonoid) {
    return function (dictMonadRec) {
        return new Control_Monad_Rec_Class.MonadRec(function () {
            return monadWriterT(dictMonoid)(dictMonadRec["__superclass_Control.Monad.Monad_0"]());
        }, function (f) {
            return function (a) {
                var f$prime = function (v) {
                    return Control_Bind.bind((dictMonadRec["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Bind.Bind_1"]())((function () {
                        var $85 = f(v.value0);
                        return $85;
                    })())(function (v1) {
                        return Control_Applicative.pure((dictMonadRec["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Applicative.Applicative_0"]())((function () {
                            if (v1.value0 instanceof Data_Either.Left) {
                                return new Data_Either.Left(new Data_Tuple.Tuple(v1.value0.value0, Data_Semigroup.append(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]())(v.value1)(v1.value1)));
                            };
                            if (v1.value0 instanceof Data_Either.Right) {
                                return new Data_Either.Right(new Data_Tuple.Tuple(v1.value0.value0, Data_Semigroup.append(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]())(v.value1)(v1.value1)));
                            };
                            throw new Error("Failed pattern match at Control.Monad.Writer.Trans line 83, column 11 - line 85, column 49: " + [ v1.value0.constructor.name ]);
                        })());
                    });
                };
                return Data_Function.apply(WriterT)(Control_Monad_Rec_Class.tailRecM(dictMonadRec)(f$prime)(new Data_Tuple.Tuple(a, Data_Monoid.mempty(dictMonoid))));
            };
        });
    };
};
var monadStateWriterT = function (dictMonoid) {
    return function (dictMonadState) {
        return new Control_Monad_State_Class.MonadState(function () {
            return monadWriterT(dictMonoid)(dictMonadState["__superclass_Control.Monad.Monad_0"]());
        }, function (f) {
            return Control_Monad_Trans.lift(monadTransWriterT(dictMonoid))(dictMonadState["__superclass_Control.Monad.Monad_0"]())(Control_Monad_State_Class.state(dictMonadState)(f));
        });
    };
};
var monadWriterWriterT = function (dictMonoid) {
    return function (dictMonad) {
        return new Control_Monad_Writer_Class.MonadWriter(function () {
            return monadWriterT(dictMonoid)(dictMonad);
        }, function (v) {
            return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(v)(function (v1) {
                return Data_Function.apply(Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]()))(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v1.value0, v1.value1), v1.value1));
            });
        }, function (v) {
            return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(v)(function (v1) {
                return Data_Function.apply(Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]()))(new Data_Tuple.Tuple(v1.value0.value0, v1.value0.value1(v1.value1)));
            });
        }, function ($107) {
            return WriterT(Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())($107));
        });
    };
};
var altWriterT = function (dictAlt) {
    return new Control_Alt.Alt(function () {
        return functorWriterT(dictAlt["__superclass_Data.Functor.Functor_0"]());
    }, function (v) {
        return function (v1) {
            return Control_Alt.alt(dictAlt)(v)(v1);
        };
    });
};
var plusWriterT = function (dictPlus) {
    return new Control_Plus.Plus(function () {
        return altWriterT(dictPlus["__superclass_Control.Alt.Alt_0"]());
    }, Control_Plus.empty(dictPlus));
};
var alternativeWriterT = function (dictMonoid) {
    return function (dictAlternative) {
        return new Control_Alternative.Alternative(function () {
            return applicativeWriterT(dictMonoid)(dictAlternative["__superclass_Control.Applicative.Applicative_0"]());
        }, function () {
            return plusWriterT(dictAlternative["__superclass_Control.Plus.Plus_1"]());
        });
    };
};
var monadZeroWriterT = function (dictMonoid) {
    return function (dictMonadZero) {
        return new Control_MonadZero.MonadZero(function () {
            return alternativeWriterT(dictMonoid)(dictMonadZero["__superclass_Control.Alternative.Alternative_1"]());
        }, function () {
            return monadWriterT(dictMonoid)(dictMonadZero["__superclass_Control.Monad.Monad_0"]());
        });
    };
};
var monadPlusWriterT = function (dictMonoid) {
    return function (dictMonadPlus) {
        return new Control_MonadPlus.MonadPlus(function () {
            return monadZeroWriterT(dictMonoid)(dictMonadPlus["__superclass_Control.MonadZero.MonadZero_0"]());
        });
    };
};
module.exports = {
    WriterT: WriterT, 
    execWriterT: execWriterT, 
    mapWriterT: mapWriterT, 
    runWriterT: runWriterT, 
    functorWriterT: functorWriterT, 
    applyWriterT: applyWriterT, 
    applicativeWriterT: applicativeWriterT, 
    altWriterT: altWriterT, 
    plusWriterT: plusWriterT, 
    alternativeWriterT: alternativeWriterT, 
    bindWriterT: bindWriterT, 
    monadWriterT: monadWriterT, 
    monadRecWriterT: monadRecWriterT, 
    monadZeroWriterT: monadZeroWriterT, 
    monadPlusWriterT: monadPlusWriterT, 
    monadTransWriterT: monadTransWriterT, 
    monadEffWriter: monadEffWriter, 
    monadContWriterT: monadContWriterT, 
    monadErrorWriterT: monadErrorWriterT, 
    monadReaderWriterT: monadReaderWriterT, 
    monadStateWriterT: monadStateWriterT, 
    monadWriterWriterT: monadWriterWriterT
};

},{"../Control.Alt":5,"../Control.Alternative":6,"../Control.Applicative":7,"../Control.Apply":9,"../Control.Bind":13,"../Control.Monad":48,"../Control.Monad.Cont.Class":22,"../Control.Monad.Eff.Class":24,"../Control.Monad.Error.Class":35,"../Control.Monad.Reader.Class":39,"../Control.Monad.Rec.Class":41,"../Control.Monad.State.Class":44,"../Control.Monad.Trans":45,"../Control.Monad.Writer.Class":46,"../Control.MonadPlus":49,"../Control.MonadZero":50,"../Control.Plus":52,"../Control.Semigroupoid":53,"../Data.Either":77,"../Data.Function":97,"../Data.Functor":100,"../Data.Monoid":120,"../Data.Semigroup":132,"../Data.Tuple":147,"../Prelude":162}],48:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Data_Functor = require("../Data.Functor");
var Monad = function (__superclass_Control$dotApplicative$dotApplicative_0, __superclass_Control$dotBind$dotBind_1) {
    this["__superclass_Control.Applicative.Applicative_0"] = __superclass_Control$dotApplicative$dotApplicative_0;
    this["__superclass_Control.Bind.Bind_1"] = __superclass_Control$dotBind$dotBind_1;
};
var monadFn = new Monad(function () {
    return Control_Applicative.applicativeFn;
}, function () {
    return Control_Bind.bindFn;
});
var monadArray = new Monad(function () {
    return Control_Applicative.applicativeArray;
}, function () {
    return Control_Bind.bindArray;
});
var liftM1 = function (dictMonad) {
    return function (f) {
        return function (a) {
            return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(a)(function (v) {
                return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())(f(v));
            });
        };
    };
};
var ap = function (dictMonad) {
    return function (f) {
        return function (a) {
            return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(f)(function (v) {
                return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(a)(function (v1) {
                    return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())(v(v1));
                });
            });
        };
    };
};
module.exports = {
    Monad: Monad, 
    ap: ap, 
    liftM1: liftM1, 
    monadFn: monadFn, 
    monadArray: monadArray
};

},{"../Control.Applicative":7,"../Control.Apply":9,"../Control.Bind":13,"../Data.Functor":100}],49:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Monad = require("../Control.Monad");
var Control_MonadZero = require("../Control.MonadZero");
var Control_Plus = require("../Control.Plus");
var Data_Functor = require("../Data.Functor");
var MonadPlus = function (__superclass_Control$dotMonadZero$dotMonadZero_0) {
    this["__superclass_Control.MonadZero.MonadZero_0"] = __superclass_Control$dotMonadZero$dotMonadZero_0;
};
var monadPlusArray = new MonadPlus(function () {
    return Control_MonadZero.monadZeroArray;
});
module.exports = {
    MonadPlus: MonadPlus, 
    monadPlusArray: monadPlusArray
};

},{"../Control.Alt":5,"../Control.Alternative":6,"../Control.Applicative":7,"../Control.Apply":9,"../Control.Bind":13,"../Control.Monad":48,"../Control.MonadZero":50,"../Control.Plus":52,"../Data.Functor":100}],50:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Monad = require("../Control.Monad");
var Control_Plus = require("../Control.Plus");
var Data_Functor = require("../Data.Functor");
var Data_Unit = require("../Data.Unit");
var MonadZero = function (__superclass_Control$dotAlternative$dotAlternative_1, __superclass_Control$dotMonad$dotMonad_0) {
    this["__superclass_Control.Alternative.Alternative_1"] = __superclass_Control$dotAlternative$dotAlternative_1;
    this["__superclass_Control.Monad.Monad_0"] = __superclass_Control$dotMonad$dotMonad_0;
};
var monadZeroArray = new MonadZero(function () {
    return Control_Alternative.alternativeArray;
}, function () {
    return Control_Monad.monadArray;
});
var guard = function (dictMonadZero) {
    return function (v) {
        if (v) {
            return Control_Applicative.pure((dictMonadZero["__superclass_Control.Alternative.Alternative_1"]())["__superclass_Control.Applicative.Applicative_0"]())(Data_Unit.unit);
        };
        if (!v) {
            return Control_Plus.empty((dictMonadZero["__superclass_Control.Alternative.Alternative_1"]())["__superclass_Control.Plus.Plus_1"]());
        };
        throw new Error("Failed pattern match at Control.MonadZero line 52, column 1 - line 52, column 23: " + [ v.constructor.name ]);
    };
};
module.exports = {
    MonadZero: MonadZero, 
    guard: guard, 
    monadZeroArray: monadZeroArray
};

},{"../Control.Alt":5,"../Control.Alternative":6,"../Control.Applicative":7,"../Control.Apply":9,"../Control.Bind":13,"../Control.Monad":48,"../Control.Plus":52,"../Data.Functor":100,"../Data.Unit":151}],51:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Apply = require("../Control.Apply");
var Control_Monad_Cont_Trans = require("../Control.Monad.Cont.Trans");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Ref = require("../Control.Monad.Eff.Ref");
var Control_Monad_Eff_Unsafe = require("../Control.Monad.Eff.Unsafe");
var Control_Monad_Except_Trans = require("../Control.Monad.Except.Trans");
var Control_Monad_Reader_Trans = require("../Control.Monad.Reader.Trans");
var Control_Monad_Maybe_Trans = require("../Control.Monad.Maybe.Trans");
var Control_Monad_Writer_Trans = require("../Control.Monad.Writer.Trans");
var Control_Plus = require("../Control.Plus");
var Data_Foldable = require("../Data.Foldable");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Control_Bind = require("../Control.Bind");
var Data_Function = require("../Data.Function");
var Data_Semigroup = require("../Data.Semigroup");
var Control_Applicative = require("../Control.Applicative");
var Data_Unit = require("../Data.Unit");
var Data_Functor = require("../Data.Functor");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Either = require("../Data.Either");
var Parallel = function (x) {
    return x;
};
var MonadPar = function (__superclass_Control$dotMonad$dotMonad_0, par) {
    this["__superclass_Control.Monad.Monad_0"] = __superclass_Control$dotMonad$dotMonad_0;
    this.par = par;
};
var MonadRace = function (__superclass_Control$dotParallel$dotClass$dotMonadPar_0, race, stall) {
    this["__superclass_Control.Parallel.Class.MonadPar_0"] = __superclass_Control$dotParallel$dotClass$dotMonadPar_0;
    this.race = race;
    this.stall = stall;
};
var unsafeWithRef = Control_Monad_Eff_Unsafe.unsafeInterleaveEff;
var stall = function (dict) {
    return dict.stall;
};
var runParallel = function (v) {
    return v;
};
var race = function (dict) {
    return dict.race;
};
var parallel = Parallel;
var par = function (dict) {
    return dict.par;
};
var monadParWriterT = function (dictMonoid) {
    return function (dictMonadPar) {
        return new MonadPar(function () {
            return Control_Monad_Writer_Trans.monadWriterT(dictMonoid)(dictMonadPar["__superclass_Control.Monad.Monad_0"]());
        }, function (f) {
            return function (v) {
                return function (v1) {
                    return Data_Function.apply(Control_Monad_Writer_Trans.WriterT)(par(dictMonadPar)(function (v2) {
                        return function (v3) {
                            return new Data_Tuple.Tuple(f(v2.value0)(v3.value0), Data_Semigroup.append(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]())(v2.value1)(v3.value1));
                        };
                    })(v)(v1));
                };
            };
        });
    };
};
var monadRaceWriterT = function (dictMonoid) {
    return function (dictMonadRace) {
        return new MonadRace(function () {
            return monadParWriterT(dictMonoid)(dictMonadRace["__superclass_Control.Parallel.Class.MonadPar_0"]());
        }, function (v) {
            return function (v1) {
                return race(dictMonadRace)(v)(v1);
            };
        }, stall(dictMonadRace));
    };
};
var monadParReaderT = function (dictMonadPar) {
    return new MonadPar(function () {
        return Control_Monad_Reader_Trans.monadReaderT(dictMonadPar["__superclass_Control.Monad.Monad_0"]());
    }, function (f) {
        return function (v) {
            return function (v1) {
                return function (r) {
                    return par(dictMonadPar)(f)(v(r))(v1(r));
                };
            };
        };
    });
};
var monadRaceReaderT = function (dictMonadRace) {
    return new MonadRace(function () {
        return monadParReaderT(dictMonadRace["__superclass_Control.Parallel.Class.MonadPar_0"]());
    }, function (v) {
        return function (v1) {
            return function (r) {
                return race(dictMonadRace)(v(r))(v1(r));
            };
        };
    }, function (v) {
        return stall(dictMonadRace);
    });
};
var monadParMaybeT = function (dictMonadPar) {
    return new MonadPar(function () {
        return Control_Monad_Maybe_Trans.monadMaybeT(dictMonadPar["__superclass_Control.Monad.Monad_0"]());
    }, function (f) {
        return function (v) {
            return function (v1) {
                return par(dictMonadPar)(Control_Apply.lift2(Data_Maybe.applyMaybe)(f))(v)(v1);
            };
        };
    });
};
var monadRaceMaybeT = function (dictMonadRace) {
    return new MonadRace(function () {
        return monadParMaybeT(dictMonadRace["__superclass_Control.Parallel.Class.MonadPar_0"]());
    }, function (v) {
        return function (v1) {
            return race(dictMonadRace)(v)(v1);
        };
    }, stall(dictMonadRace));
};
var monadParExceptT = function (dictMonadPar) {
    return new MonadPar(function () {
        return Control_Monad_Except_Trans.monadExceptT(dictMonadPar["__superclass_Control.Monad.Monad_0"]());
    }, function (f) {
        return function (v) {
            return function (v1) {
                return par(dictMonadPar)(Control_Apply.lift2(Data_Either.applyEither)(f))(v)(v1);
            };
        };
    });
};
var monadRaceExceptT = function (dictMonadRace) {
    return new MonadRace(function () {
        return monadParExceptT(dictMonadRace["__superclass_Control.Parallel.Class.MonadPar_0"]());
    }, function (v) {
        return function (v1) {
            return race(dictMonadRace)(v)(v1);
        };
    }, stall(dictMonadRace));
};
var monadParContT = new MonadPar(function () {
    return Control_Monad_Cont_Trans.monadContT(Control_Monad_Eff.monadEff);
}, function (f) {
    return function (ca) {
        return function (cb) {
            return function (k) {
                return function __do() {
                    var v = unsafeWithRef(Control_Monad_Eff_Ref.newRef(Data_Maybe.Nothing.value))();
                    var v1 = unsafeWithRef(Control_Monad_Eff_Ref.newRef(Data_Maybe.Nothing.value))();
                    Control_Monad_Cont_Trans.runContT(ca)(function (a) {
                        return function __do() {
                            var v2 = unsafeWithRef(Control_Monad_Eff_Ref.readRef(v1))();
                            if (v2 instanceof Data_Maybe.Nothing) {
                                return unsafeWithRef(Control_Monad_Eff_Ref.writeRef(v)(new Data_Maybe.Just(a)))();
                            };
                            if (v2 instanceof Data_Maybe.Just) {
                                return k(f(a)(v2.value0))();
                            };
                            throw new Error("Failed pattern match at Control.Parallel.Class line 51, column 7 - line 53, column 28: " + [ v2.constructor.name ]);
                        };
                    })();
                    return Control_Monad_Cont_Trans.runContT(cb)(function (b) {
                        return function __do() {
                            var v2 = unsafeWithRef(Control_Monad_Eff_Ref.readRef(v))();
                            if (v2 instanceof Data_Maybe.Nothing) {
                                return unsafeWithRef(Control_Monad_Eff_Ref.writeRef(v1)(new Data_Maybe.Just(b)))();
                            };
                            if (v2 instanceof Data_Maybe.Just) {
                                return k(f(v2.value0)(b))();
                            };
                            throw new Error("Failed pattern match at Control.Parallel.Class line 57, column 7 - line 59, column 28: " + [ v2.constructor.name ]);
                        };
                    })();
                };
            };
        };
    };
});
var monadRaceContT = new MonadRace(function () {
    return monadParContT;
}, function (c1) {
    return function (c2) {
        return function (k) {
            return function __do() {
                var v = unsafeWithRef(Control_Monad_Eff_Ref.newRef(false))();
                Control_Monad_Cont_Trans.runContT(c1)(function (a) {
                    return function __do() {
                        var v1 = unsafeWithRef(Control_Monad_Eff_Ref.readRef(v))();
                        if (v1) {
                            return Data_Unit.unit;
                        };
                        if (!v1) {
                            unsafeWithRef(Control_Monad_Eff_Ref.writeRef(v)(true))();
                            return k(a)();
                        };
                        throw new Error("Failed pattern match at Control.Parallel.Class line 111, column 7 - line 115, column 14: " + [ v1.constructor.name ]);
                    };
                })();
                return Control_Monad_Cont_Trans.runContT(c2)(function (a) {
                    return function __do() {
                        var v1 = unsafeWithRef(Control_Monad_Eff_Ref.readRef(v))();
                        if (v1) {
                            return Data_Unit.unit;
                        };
                        if (!v1) {
                            unsafeWithRef(Control_Monad_Eff_Ref.writeRef(v)(true))();
                            return k(a)();
                        };
                        throw new Error("Failed pattern match at Control.Parallel.Class line 119, column 7 - line 123, column 14: " + [ v1.constructor.name ]);
                    };
                })();
            };
        };
    };
}, function (v) {
    return Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(Data_Unit.unit);
});
var functorParallel = function (dictFunctor) {
    return new Data_Functor.Functor(function (f) {
        return function ($90) {
            return parallel(Data_Functor.map(dictFunctor)(f)(runParallel($90)));
        };
    });
};
var applyParallel = function (dictMonadPar) {
    return new Control_Apply.Apply(function () {
        return functorParallel((((dictMonadPar["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Bind.Bind_1"]())["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]());
    }, function (f) {
        return function (a) {
            return parallel(par(dictMonadPar)(Data_Function.apply)(runParallel(f))(runParallel(a)));
        };
    });
};
var applicativeParallel = function (dictMonadPar) {
    return new Control_Applicative.Applicative(function () {
        return applyParallel(dictMonadPar);
    }, function ($91) {
        return parallel(Control_Applicative.pure((dictMonadPar["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Applicative.Applicative_0"]())($91));
    });
};
var parTraverse = function (dictMonadPar) {
    return function (dictTraversable) {
        return function (f) {
            return function ($92) {
                return runParallel(Data_Traversable.traverse(dictTraversable)(applicativeParallel(dictMonadPar))(function ($93) {
                    return Parallel(f($93));
                })($92));
            };
        };
    };
};
var parTraverse_ = function (dictMonadPar) {
    return function (dictFoldable) {
        return function (f) {
            return function ($94) {
                return runParallel(Data_Foldable.traverse_(applicativeParallel(dictMonadPar))(dictFoldable)(function ($95) {
                    return Parallel(f($95));
                })($94));
            };
        };
    };
};
var altParallel = function (dictMonadRace) {
    return new Control_Alt.Alt(function () {
        return functorParallel(((((dictMonadRace["__superclass_Control.Parallel.Class.MonadPar_0"]())["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Bind.Bind_1"]())["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]());
    }, function (a) {
        return function (b) {
            return parallel(race(dictMonadRace)(runParallel(a))(runParallel(b)));
        };
    });
};
var plusParallel = function (dictMonadRace) {
    return new Control_Plus.Plus(function () {
        return altParallel(dictMonadRace);
    }, parallel(stall(dictMonadRace)));
};
var alternativeParallel = function (dictMonadRace) {
    return new Control_Alternative.Alternative(function () {
        return applicativeParallel(dictMonadRace["__superclass_Control.Parallel.Class.MonadPar_0"]());
    }, function () {
        return plusParallel(dictMonadRace);
    });
};
module.exports = {
    MonadPar: MonadPar, 
    MonadRace: MonadRace, 
    par: par, 
    parTraverse: parTraverse, 
    parTraverse_: parTraverse_, 
    parallel: parallel, 
    race: race, 
    runParallel: runParallel, 
    stall: stall, 
    monadParContT: monadParContT, 
    monadParExceptT: monadParExceptT, 
    monadParMaybeT: monadParMaybeT, 
    monadParReaderT: monadParReaderT, 
    monadParWriterT: monadParWriterT, 
    monadRaceContT: monadRaceContT, 
    monadRaceExceptT: monadRaceExceptT, 
    monadRaceMaybeT: monadRaceMaybeT, 
    monadRaceReaderT: monadRaceReaderT, 
    monadRaceWriterT: monadRaceWriterT, 
    functorParallel: functorParallel, 
    applyParallel: applyParallel, 
    applicativeParallel: applicativeParallel, 
    altParallel: altParallel, 
    plusParallel: plusParallel, 
    alternativeParallel: alternativeParallel
};

},{"../Control.Alt":5,"../Control.Alternative":6,"../Control.Applicative":7,"../Control.Apply":9,"../Control.Bind":13,"../Control.Monad.Cont.Trans":23,"../Control.Monad.Eff":34,"../Control.Monad.Eff.Ref":30,"../Control.Monad.Eff.Unsafe":32,"../Control.Monad.Except.Trans":36,"../Control.Monad.Maybe.Trans":37,"../Control.Monad.Reader.Trans":40,"../Control.Monad.Writer.Trans":47,"../Control.Plus":52,"../Control.Semigroupoid":53,"../Data.Either":77,"../Data.Foldable":84,"../Data.Function":97,"../Data.Functor":100,"../Data.Maybe":113,"../Data.Monoid":120,"../Data.Semigroup":132,"../Data.Traversable":146,"../Data.Tuple":147,"../Data.Unit":151,"../Prelude":162}],52:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Alt = require("../Control.Alt");
var Data_Functor = require("../Data.Functor");
var Plus = function (__superclass_Control$dotAlt$dotAlt_0, empty) {
    this["__superclass_Control.Alt.Alt_0"] = __superclass_Control$dotAlt$dotAlt_0;
    this.empty = empty;
};
var plusArray = new Plus(function () {
    return Control_Alt.altArray;
}, [  ]);
var empty = function (dict) {
    return dict.empty;
};
module.exports = {
    Plus: Plus, 
    empty: empty, 
    plusArray: plusArray
};

},{"../Control.Alt":5,"../Data.Functor":100}],53:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Semigroupoid = function (compose) {
    this.compose = compose;
};
var semigroupoidFn = new Semigroupoid(function (f) {
    return function (g) {
        return function (x) {
            return f(g(x));
        };
    };
});
var compose = function (dict) {
    return dict.compose;
};
var composeFlipped = function (dictSemigroupoid) {
    return function (f) {
        return function (g) {
            return compose(dictSemigroupoid)(g)(f);
        };
    };
};
module.exports = {
    Semigroupoid: Semigroupoid, 
    compose: compose, 
    composeFlipped: composeFlipped, 
    semigroupoidFn: semigroupoidFn
};

},{}],54:[function(require,module,exports){
/* global EventTarget */
"use strict";

exports._readEventTarget = function (left) {
  return function (right) {
    return function (foreign) {
      return foreign instanceof EventTarget ? left("Value is not an EventTarget") : right(foreign);
    };
  };
};

},{}],55:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Either = require("../Data.Either");
var Data_Foreign = require("../Data.Foreign");
var Data_Foreign_Class = require("../Data.Foreign.Class");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var Data_Eq = require("../Data.Eq");
var Data_Ord = require("../Data.Ord");
var EventType = function (x) {
    return x;
};
var wheelEventToEvent = Unsafe_Coerce.unsafeCoerce;
var uiEventToEvent = Unsafe_Coerce.unsafeCoerce;
var touchEventToEvent = Unsafe_Coerce.unsafeCoerce;
var readWheelEvent = Data_Foreign.unsafeReadTagged("WheelEvent");
var readUIEvent = Data_Foreign.unsafeReadTagged("UIEvent");
var readTouchEvent = Data_Foreign.unsafeReadTagged("TouchEvent");
var readProgressEvent = Data_Foreign.unsafeReadTagged("ProgressEvent");
var readMouseEvent = Data_Foreign.unsafeReadTagged("MouseEvent");
var readKeyboardEvent = Data_Foreign.unsafeReadTagged("KeyboardEvent");
var readInputEvent = Data_Foreign.unsafeReadTagged("InputEvent");
var readFocusEvent = Data_Foreign.unsafeReadTagged("FocusEvent");
var readEventTarget = $foreign._readEventTarget(Data_Either.Left.create)(Data_Either.Right.create);
var readCustomEvent = Data_Foreign.unsafeReadTagged("CustomEvent");
var readCompositionEvent = Data_Foreign.unsafeReadTagged("CompositionEvent");
var progressEventToEvent = Unsafe_Coerce.unsafeCoerce;
var mouseEventToEvent = Unsafe_Coerce.unsafeCoerce;
var keyboardEventToEvent = Unsafe_Coerce.unsafeCoerce;
var isForeignWheelEvent = new Data_Foreign_Class.IsForeign(readWheelEvent);
var isForeignUIEvent = new Data_Foreign_Class.IsForeign(readUIEvent);
var isForeignTouchEvent = new Data_Foreign_Class.IsForeign(readTouchEvent);
var isForeignProgressEvent = new Data_Foreign_Class.IsForeign(readProgressEvent);
var isForeignMouseEvent = new Data_Foreign_Class.IsForeign(readMouseEvent);
var isForeignKeyboardEvent = new Data_Foreign_Class.IsForeign(readKeyboardEvent);
var isForeignInputEvent = new Data_Foreign_Class.IsForeign(readInputEvent);
var isForeignFocusEvent = new Data_Foreign_Class.IsForeign(readFocusEvent);
var isForeignEventTarget = new Data_Foreign_Class.IsForeign(readEventTarget);
var isForeignCustomEvent = new Data_Foreign_Class.IsForeign(readCustomEvent);
var isForeignCompositionEvent = new Data_Foreign_Class.IsForeign(readCompositionEvent);
var inputEventToEvent = Unsafe_Coerce.unsafeCoerce;
var focusEventToEvent = Unsafe_Coerce.unsafeCoerce;
var eqEventType = new Data_Eq.Eq(function (x) {
    return function (y) {
        return x === y;
    };
});
var ordEventType = new Data_Ord.Ord(function () {
    return eqEventType;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(Data_Ord.ordString)(x)(y);
    };
});
var customEventToEvent = Unsafe_Coerce.unsafeCoerce;
var compositionEventToEvent = Unsafe_Coerce.unsafeCoerce;
module.exports = {
    EventType: EventType, 
    compositionEventToEvent: compositionEventToEvent, 
    customEventToEvent: customEventToEvent, 
    focusEventToEvent: focusEventToEvent, 
    inputEventToEvent: inputEventToEvent, 
    keyboardEventToEvent: keyboardEventToEvent, 
    mouseEventToEvent: mouseEventToEvent, 
    progressEventToEvent: progressEventToEvent, 
    readCompositionEvent: readCompositionEvent, 
    readCustomEvent: readCustomEvent, 
    readEventTarget: readEventTarget, 
    readFocusEvent: readFocusEvent, 
    readInputEvent: readInputEvent, 
    readKeyboardEvent: readKeyboardEvent, 
    readMouseEvent: readMouseEvent, 
    readProgressEvent: readProgressEvent, 
    readTouchEvent: readTouchEvent, 
    readUIEvent: readUIEvent, 
    readWheelEvent: readWheelEvent, 
    touchEventToEvent: touchEventToEvent, 
    uiEventToEvent: uiEventToEvent, 
    wheelEventToEvent: wheelEventToEvent, 
    eqEventType: eqEventType, 
    ordEventType: ordEventType, 
    isForeignEventTarget: isForeignEventTarget, 
    isForeignCustomEvent: isForeignCustomEvent, 
    isForeignUIEvent: isForeignUIEvent, 
    isForeignFocusEvent: isForeignFocusEvent, 
    isForeignMouseEvent: isForeignMouseEvent, 
    isForeignWheelEvent: isForeignWheelEvent, 
    isForeignTouchEvent: isForeignTouchEvent, 
    isForeignInputEvent: isForeignInputEvent, 
    isForeignKeyboardEvent: isForeignKeyboardEvent, 
    isForeignCompositionEvent: isForeignCompositionEvent, 
    isForeignProgressEvent: isForeignProgressEvent
};

},{"../Data.Either":77,"../Data.Eq":79,"../Data.Foreign":94,"../Data.Foreign.Class":85,"../Data.Ord":127,"../Prelude":162,"../Unsafe.Coerce":165,"./foreign":54}],56:[function(require,module,exports){
"use strict";

exports._readHTMLElement = function (failure) {
  return function (success) {
    return function (value) {
      var tag = Object.prototype.toString.call(value);
      if (tag.indexOf("[object HTML") === 0 && tag.indexOf("Element]") === tag.length - 8) {
        return success(value);
      } else {
        return failure(tag);
      }
    };
  };
};

},{}],57:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Either = require("../Data.Either");
var Data_Foreign = require("../Data.Foreign");
var Data_Foreign_Class = require("../Data.Foreign.Class");
var DOM_Event_Types = require("../DOM.Event.Types");
var DOM_Node_Types = require("../DOM.Node.Types");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var windowToEventTarget = Unsafe_Coerce.unsafeCoerce;
var readHTMLVideoElement = Data_Foreign.unsafeReadTagged("HTMLVideoElement");
var readHTMLUListElement = Data_Foreign.unsafeReadTagged("HTMLUListElement");
var readHTMLTrackElement = Data_Foreign.unsafeReadTagged("HTMLTrackElement");
var readHTMLTitleElement = Data_Foreign.unsafeReadTagged("HTMLTitleElement");
var readHTMLTimeElement = Data_Foreign.unsafeReadTagged("HTMLTimeElement");
var readHTMLTextAreaElement = Data_Foreign.unsafeReadTagged("HTMLTextAreaElement");
var readHTMLTemplateElement = Data_Foreign.unsafeReadTagged("HTMLTemplateElement");
var readHTMLTableSectionElement = Data_Foreign.unsafeReadTagged("HTMLTableSectionElement");
var readHTMLTableRowElement = Data_Foreign.unsafeReadTagged("HTMLTableRowElement");
var readHTMLTableHeaderCellElement = Data_Foreign.unsafeReadTagged("HTMLTableHeaderCellElement");
var readHTMLTableElement = Data_Foreign.unsafeReadTagged("HTMLTableElement");
var readHTMLTableDataCellElement = Data_Foreign.unsafeReadTagged("HTMLTableDataCellElement");
var readHTMLTableColElement = Data_Foreign.unsafeReadTagged("HTMLTableColElement");
var readHTMLTableCellElement = Data_Foreign.unsafeReadTagged("HTMLTableCellElement");
var readHTMLTableCaptionElement = Data_Foreign.unsafeReadTagged("HTMLTableCaptionElement");
var readHTMLStyleElement = Data_Foreign.unsafeReadTagged("HTMLStyleElement");
var readHTMLSpanElement = Data_Foreign.unsafeReadTagged("HTMLSpanElement");
var readHTMLSourceElement = Data_Foreign.unsafeReadTagged("HTMLSourceElement");
var readHTMLSelectElement = Data_Foreign.unsafeReadTagged("HTMLSelectElement");
var readHTMLScriptElement = Data_Foreign.unsafeReadTagged("HTMLScriptElement");
var readHTMLQuoteElement = Data_Foreign.unsafeReadTagged("HTMLQuoteElement");
var readHTMLProgressElement = Data_Foreign.unsafeReadTagged("HTMLProgressElement");
var readHTMLPreElement = Data_Foreign.unsafeReadTagged("HTMLPreElement");
var readHTMLParamElement = Data_Foreign.unsafeReadTagged("HTMLParamElement");
var readHTMLParagraphElement = Data_Foreign.unsafeReadTagged("HTMLParagraphElement");
var readHTMLOutputElement = Data_Foreign.unsafeReadTagged("HTMLOutputElement");
var readHTMLOptionElement = Data_Foreign.unsafeReadTagged("HTMLOptionElement");
var readHTMLOptGroupElement = Data_Foreign.unsafeReadTagged("HTMLOptGroupElement");
var readHTMLObjectElement = Data_Foreign.unsafeReadTagged("HTMLObjectElement");
var readHTMLOListElement = Data_Foreign.unsafeReadTagged("HTMLOListElement");
var readHTMLModElement = Data_Foreign.unsafeReadTagged("HTMLModElement");
var readHTMLMeterElement = Data_Foreign.unsafeReadTagged("HTMLMeterElement");
var readHTMLMetaElement = Data_Foreign.unsafeReadTagged("HTMLMetaElement");
var readHTMLMediaElement = Data_Foreign.unsafeReadTagged("HTMLMediaElement");
var readHTMLMapElement = Data_Foreign.unsafeReadTagged("HTMLMapElement");
var readHTMLLinkElement = Data_Foreign.unsafeReadTagged("HTMLLinkElement");
var readHTMLLegendElement = Data_Foreign.unsafeReadTagged("HTMLLegendElement");
var readHTMLLabelElement = Data_Foreign.unsafeReadTagged("HTMLLabelElement");
var readHTMLLIElement = Data_Foreign.unsafeReadTagged("HTMLLIElement");
var readHTMLKeygenElement = Data_Foreign.unsafeReadTagged("HTMLKeygenElement");
var readHTMLInputElement = Data_Foreign.unsafeReadTagged("HTMLInputElement");
var readHTMLImageElement = Data_Foreign.unsafeReadTagged("HTMLImageElement");
var readHTMLIFrameElement = Data_Foreign.unsafeReadTagged("HTMLIFrameElement");
var readHTMLHtmlElement = Data_Foreign.unsafeReadTagged("HTMLHtmlElement");
var readHTMLHeadingElement = Data_Foreign.unsafeReadTagged("HTMLHeadingElement");
var readHTMLHeadElement = Data_Foreign.unsafeReadTagged("HTMLHeadElement");
var readHTMLHRElement = Data_Foreign.unsafeReadTagged("HTMLHRElement");
var readHTMLFormElement = Data_Foreign.unsafeReadTagged("HTMLFormElement");
var readHTMLFieldSetElement = Data_Foreign.unsafeReadTagged("HTMLFieldSetElement");
var readHTMLEmbedElement = Data_Foreign.unsafeReadTagged("HTMLEmbedElement");
var readHTMLElement = $foreign._readHTMLElement(function ($0) {
    return Data_Either.Left.create(Data_Foreign.TypeMismatch.create("HTMLElement")($0));
})(Data_Either.Right.create);
var readHTMLDocument = Data_Foreign.unsafeReadTagged("HTMLDocument");
var readHTMLDivElement = Data_Foreign.unsafeReadTagged("HTMLDivElement");
var readHTMLDataListElement = Data_Foreign.unsafeReadTagged("HTMLDataListElement");
var readHTMLDataElement = Data_Foreign.unsafeReadTagged("HTMLDataElement");
var readHTMLDListElement = Data_Foreign.unsafeReadTagged("HTMLDListElement");
var readHTMLCanvasElement = Data_Foreign.unsafeReadTagged("HTMLCanvasElement");
var readHTMLButtonElement = Data_Foreign.unsafeReadTagged("HTMLButtonElement");
var readHTMLBodyElement = Data_Foreign.unsafeReadTagged("HTMLBodyElement");
var readHTMLBaseElement = Data_Foreign.unsafeReadTagged("HTMLBaseElement");
var readHTMLBRElement = Data_Foreign.unsafeReadTagged("HTMLBRElement");
var readHTMLAudioElement = Data_Foreign.unsafeReadTagged("HTMLAudioElement");
var readHTMLAreaElement = Data_Foreign.unsafeReadTagged("HTMLAreaElement");
var readHTMLAnchorElement = Data_Foreign.unsafeReadTagged("HTMLAnchorElement");
var isForeignHTMLUListElement = new Data_Foreign_Class.IsForeign(readHTMLUListElement);
var isForeignHTMLTrackElement = new Data_Foreign_Class.IsForeign(readHTMLTrackElement);
var isForeignHTMLTitleElement = new Data_Foreign_Class.IsForeign(readHTMLTitleElement);
var isForeignHTMLTimeElement = new Data_Foreign_Class.IsForeign(readHTMLTimeElement);
var isForeignHTMLTextAreaElement = new Data_Foreign_Class.IsForeign(readHTMLTextAreaElement);
var isForeignHTMLTemplateElement = new Data_Foreign_Class.IsForeign(readHTMLTemplateElement);
var isForeignHTMLTableSectionElement = new Data_Foreign_Class.IsForeign(readHTMLTableSectionElement);
var isForeignHTMLTableRowElement = new Data_Foreign_Class.IsForeign(readHTMLTableRowElement);
var isForeignHTMLTableElement = new Data_Foreign_Class.IsForeign(readHTMLTableElement);
var isForeignHTMLTableColElement = new Data_Foreign_Class.IsForeign(readHTMLTableColElement);
var isForeignHTMLTableCaptionElement = new Data_Foreign_Class.IsForeign(readHTMLTableCaptionElement);
var isForeignHTMLStyleElement = new Data_Foreign_Class.IsForeign(readHTMLStyleElement);
var isForeignHTMLSpanElement = new Data_Foreign_Class.IsForeign(readHTMLSpanElement);
var isForeignHTMLSourceElement = new Data_Foreign_Class.IsForeign(readHTMLSourceElement);
var isForeignHTMLSelectElement = new Data_Foreign_Class.IsForeign(readHTMLSelectElement);
var isForeignHTMLScriptElement = new Data_Foreign_Class.IsForeign(readHTMLScriptElement);
var isForeignHTMLQuoteElement = new Data_Foreign_Class.IsForeign(readHTMLQuoteElement);
var isForeignHTMLProgressElement = new Data_Foreign_Class.IsForeign(readHTMLProgressElement);
var isForeignHTMLPreElement = new Data_Foreign_Class.IsForeign(readHTMLPreElement);
var isForeignHTMLParamElement = new Data_Foreign_Class.IsForeign(readHTMLParamElement);
var isForeignHTMLParagraphElement = new Data_Foreign_Class.IsForeign(readHTMLParagraphElement);
var isForeignHTMLOutputElement = new Data_Foreign_Class.IsForeign(readHTMLOutputElement);
var isForeignHTMLOptionElement = new Data_Foreign_Class.IsForeign(readHTMLOptionElement);
var isForeignHTMLOptGroupElement = new Data_Foreign_Class.IsForeign(readHTMLOptGroupElement);
var isForeignHTMLObjectElement = new Data_Foreign_Class.IsForeign(readHTMLObjectElement);
var isForeignHTMLOListElement = new Data_Foreign_Class.IsForeign(readHTMLOListElement);
var isForeignHTMLModElement = new Data_Foreign_Class.IsForeign(readHTMLModElement);
var isForeignHTMLMeterElement = new Data_Foreign_Class.IsForeign(readHTMLMeterElement);
var isForeignHTMLMetaElement = new Data_Foreign_Class.IsForeign(readHTMLMetaElement);
var isForeignHTMLMapElement = new Data_Foreign_Class.IsForeign(readHTMLMapElement);
var isForeignHTMLLinkElement = new Data_Foreign_Class.IsForeign(readHTMLLinkElement);
var isForeignHTMLLegendElement = new Data_Foreign_Class.IsForeign(readHTMLLegendElement);
var isForeignHTMLLabelElement = new Data_Foreign_Class.IsForeign(readHTMLLabelElement);
var isForeignHTMLLIElement = new Data_Foreign_Class.IsForeign(readHTMLLIElement);
var isForeignHTMLKeygenElement = new Data_Foreign_Class.IsForeign(readHTMLKeygenElement);
var isForeignHTMLInputElement = new Data_Foreign_Class.IsForeign(readHTMLInputElement);
var isForeignHTMLImageElement = new Data_Foreign_Class.IsForeign(readHTMLImageElement);
var isForeignHTMLIFrameElement = new Data_Foreign_Class.IsForeign(readHTMLIFrameElement);
var isForeignHTMLHtmlElement = new Data_Foreign_Class.IsForeign(readHTMLHtmlElement);
var isForeignHTMLHeadingElement = new Data_Foreign_Class.IsForeign(readHTMLHeadingElement);
var isForeignHTMLHeadElement = new Data_Foreign_Class.IsForeign(readHTMLHeadElement);
var isForeignHTMLHRElement = new Data_Foreign_Class.IsForeign(readHTMLHRElement);
var isForeignHTMLFormElement = new Data_Foreign_Class.IsForeign(readHTMLFormElement);
var isForeignHTMLFieldSetElement = new Data_Foreign_Class.IsForeign(readHTMLFieldSetElement);
var isForeignHTMLEmbedElement = new Data_Foreign_Class.IsForeign(readHTMLEmbedElement);
var isForeignHTMLElement = new Data_Foreign_Class.IsForeign(readHTMLElement);
var isForeignHTMLDocument = new Data_Foreign_Class.IsForeign(readHTMLDocument);
var isForeignHTMLDivElement = new Data_Foreign_Class.IsForeign(readHTMLDivElement);
var isForeignHTMLDataListElement = new Data_Foreign_Class.IsForeign(readHTMLDataListElement);
var isForeignHTMLDataElement = new Data_Foreign_Class.IsForeign(readHTMLDataElement);
var isForeignHTMLDListElement = new Data_Foreign_Class.IsForeign(readHTMLDListElement);
var isForeignHTMLCanvasElement = new Data_Foreign_Class.IsForeign(readHTMLCanvasElement);
var isForeignHTMLButtonElement = new Data_Foreign_Class.IsForeign(readHTMLButtonElement);
var isForeignHTMLBodyElement = new Data_Foreign_Class.IsForeign(readHTMLBodyElement);
var isForeignHTMLBaseElement = new Data_Foreign_Class.IsForeign(readHTMLBaseElement);
var isForeignHTMLBRElement = new Data_Foreign_Class.IsForeign(readHTMLBRElement);
var isForeignHTMLAreaElement = new Data_Foreign_Class.IsForeign(readHTMLAreaElement);
var isForeignHTMLAnchorElement = new Data_Foreign_Class.IsForeign(readHTMLAnchorElement);
var htmlVideoElementToHTMLMediaElement = Unsafe_Coerce.unsafeCoerce;
var htmlUListElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTrackElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTitleElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTimeElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTextAreaElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTemplateElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTableSectionElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTableRowElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTableHeaderCellElementToHTMLTableCellElement = Unsafe_Coerce.unsafeCoerce;
var htmlTableElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTableDataCellElementToHTMLTableCellElement = Unsafe_Coerce.unsafeCoerce;
var htmlTableColElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTableCellElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTableCaptionElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlStyleElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlSpanElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlSourceElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlSelectElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlScriptElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlQuoteElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlProgressElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlPreElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlParamElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlParagraphElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlOutputElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlOptionElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlOptGroupElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlObjectElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlOListElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlModElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlMeterElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlMetaElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlMediaElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlMapElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlLinkElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlLegendElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlLabelElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlLIElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlKeygenElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlInputElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlImageElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlIFrameElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlHtmlElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlHeadingElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlHeadElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlHRElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlFormElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlFieldSetElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlEmbedElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlElementToParentNode = Unsafe_Coerce.unsafeCoerce;
var htmlElementToNonDocumentTypeChildNode = Unsafe_Coerce.unsafeCoerce;
var htmlElementToNode = Unsafe_Coerce.unsafeCoerce;
var htmlElementToEventTarget = Unsafe_Coerce.unsafeCoerce;
var htmlElementToElement = Unsafe_Coerce.unsafeCoerce;
var htmlDocumentToParentNode = Unsafe_Coerce.unsafeCoerce;
var htmlDocumentToNonElementParentNode = Unsafe_Coerce.unsafeCoerce;
var htmlDocumentToNode = Unsafe_Coerce.unsafeCoerce;
var htmlDocumentToEventTarget = Unsafe_Coerce.unsafeCoerce;
var htmlDocumentToDocument = Unsafe_Coerce.unsafeCoerce;
var htmlDivElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlDataListElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlDataElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlDListElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlCanvasElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlButtonElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlBodyElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlBaseElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlBRElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlAudioElementToHTMLMediaElement = Unsafe_Coerce.unsafeCoerce;
var htmlAreaElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlAnchorElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
module.exports = {
    htmlAnchorElementToHTMLElement: htmlAnchorElementToHTMLElement, 
    htmlAreaElementToHTMLElement: htmlAreaElementToHTMLElement, 
    htmlAudioElementToHTMLMediaElement: htmlAudioElementToHTMLMediaElement, 
    htmlBRElementToHTMLElement: htmlBRElementToHTMLElement, 
    htmlBaseElementToHTMLElement: htmlBaseElementToHTMLElement, 
    htmlBodyElementToHTMLElement: htmlBodyElementToHTMLElement, 
    htmlButtonElementToHTMLElement: htmlButtonElementToHTMLElement, 
    htmlCanvasElementToHTMLElement: htmlCanvasElementToHTMLElement, 
    htmlDListElementToHTMLElement: htmlDListElementToHTMLElement, 
    htmlDataElementToHTMLElement: htmlDataElementToHTMLElement, 
    htmlDataListElementToHTMLElement: htmlDataListElementToHTMLElement, 
    htmlDivElementToHTMLElement: htmlDivElementToHTMLElement, 
    htmlDocumentToDocument: htmlDocumentToDocument, 
    htmlDocumentToEventTarget: htmlDocumentToEventTarget, 
    htmlDocumentToNode: htmlDocumentToNode, 
    htmlDocumentToNonElementParentNode: htmlDocumentToNonElementParentNode, 
    htmlDocumentToParentNode: htmlDocumentToParentNode, 
    htmlElementToElement: htmlElementToElement, 
    htmlElementToEventTarget: htmlElementToEventTarget, 
    htmlElementToNode: htmlElementToNode, 
    htmlElementToNonDocumentTypeChildNode: htmlElementToNonDocumentTypeChildNode, 
    htmlElementToParentNode: htmlElementToParentNode, 
    htmlEmbedElementToHTMLElement: htmlEmbedElementToHTMLElement, 
    htmlFieldSetElementToHTMLElement: htmlFieldSetElementToHTMLElement, 
    htmlFormElementToHTMLElement: htmlFormElementToHTMLElement, 
    htmlHRElementToHTMLElement: htmlHRElementToHTMLElement, 
    htmlHeadElementToHTMLElement: htmlHeadElementToHTMLElement, 
    htmlHeadingElementToHTMLElement: htmlHeadingElementToHTMLElement, 
    htmlHtmlElementToHTMLElement: htmlHtmlElementToHTMLElement, 
    htmlIFrameElementToHTMLElement: htmlIFrameElementToHTMLElement, 
    htmlImageElementToHTMLElement: htmlImageElementToHTMLElement, 
    htmlInputElementToHTMLElement: htmlInputElementToHTMLElement, 
    htmlKeygenElementToHTMLElement: htmlKeygenElementToHTMLElement, 
    htmlLIElementToHTMLElement: htmlLIElementToHTMLElement, 
    htmlLabelElementToHTMLElement: htmlLabelElementToHTMLElement, 
    htmlLegendElementToHTMLElement: htmlLegendElementToHTMLElement, 
    htmlLinkElementToHTMLElement: htmlLinkElementToHTMLElement, 
    htmlMapElementToHTMLElement: htmlMapElementToHTMLElement, 
    htmlMediaElementToHTMLElement: htmlMediaElementToHTMLElement, 
    htmlMetaElementToHTMLElement: htmlMetaElementToHTMLElement, 
    htmlMeterElementToHTMLElement: htmlMeterElementToHTMLElement, 
    htmlModElementToHTMLElement: htmlModElementToHTMLElement, 
    htmlOListElementToHTMLElement: htmlOListElementToHTMLElement, 
    htmlObjectElementToHTMLElement: htmlObjectElementToHTMLElement, 
    htmlOptGroupElementToHTMLElement: htmlOptGroupElementToHTMLElement, 
    htmlOptionElementToHTMLElement: htmlOptionElementToHTMLElement, 
    htmlOutputElementToHTMLElement: htmlOutputElementToHTMLElement, 
    htmlParagraphElementToHTMLElement: htmlParagraphElementToHTMLElement, 
    htmlParamElementToHTMLElement: htmlParamElementToHTMLElement, 
    htmlPreElementToHTMLElement: htmlPreElementToHTMLElement, 
    htmlProgressElementToHTMLElement: htmlProgressElementToHTMLElement, 
    htmlQuoteElementToHTMLElement: htmlQuoteElementToHTMLElement, 
    htmlScriptElementToHTMLElement: htmlScriptElementToHTMLElement, 
    htmlSelectElementToHTMLElement: htmlSelectElementToHTMLElement, 
    htmlSourceElementToHTMLElement: htmlSourceElementToHTMLElement, 
    htmlSpanElementToHTMLElement: htmlSpanElementToHTMLElement, 
    htmlStyleElementToHTMLElement: htmlStyleElementToHTMLElement, 
    htmlTableCaptionElementToHTMLElement: htmlTableCaptionElementToHTMLElement, 
    htmlTableCellElementToHTMLElement: htmlTableCellElementToHTMLElement, 
    htmlTableColElementToHTMLElement: htmlTableColElementToHTMLElement, 
    htmlTableDataCellElementToHTMLTableCellElement: htmlTableDataCellElementToHTMLTableCellElement, 
    htmlTableElementToHTMLElement: htmlTableElementToHTMLElement, 
    htmlTableHeaderCellElementToHTMLTableCellElement: htmlTableHeaderCellElementToHTMLTableCellElement, 
    htmlTableRowElementToHTMLElement: htmlTableRowElementToHTMLElement, 
    htmlTableSectionElementToHTMLElement: htmlTableSectionElementToHTMLElement, 
    htmlTemplateElementToHTMLElement: htmlTemplateElementToHTMLElement, 
    htmlTextAreaElementToHTMLElement: htmlTextAreaElementToHTMLElement, 
    htmlTimeElementToHTMLElement: htmlTimeElementToHTMLElement, 
    htmlTitleElementToHTMLElement: htmlTitleElementToHTMLElement, 
    htmlTrackElementToHTMLElement: htmlTrackElementToHTMLElement, 
    htmlUListElementToHTMLElement: htmlUListElementToHTMLElement, 
    htmlVideoElementToHTMLMediaElement: htmlVideoElementToHTMLMediaElement, 
    readHTMLAnchorElement: readHTMLAnchorElement, 
    readHTMLAreaElement: readHTMLAreaElement, 
    readHTMLAudioElement: readHTMLAudioElement, 
    readHTMLBRElement: readHTMLBRElement, 
    readHTMLBaseElement: readHTMLBaseElement, 
    readHTMLBodyElement: readHTMLBodyElement, 
    readHTMLButtonElement: readHTMLButtonElement, 
    readHTMLCanvasElement: readHTMLCanvasElement, 
    readHTMLDListElement: readHTMLDListElement, 
    readHTMLDataElement: readHTMLDataElement, 
    readHTMLDataListElement: readHTMLDataListElement, 
    readHTMLDivElement: readHTMLDivElement, 
    readHTMLDocument: readHTMLDocument, 
    readHTMLElement: readHTMLElement, 
    readHTMLEmbedElement: readHTMLEmbedElement, 
    readHTMLFieldSetElement: readHTMLFieldSetElement, 
    readHTMLFormElement: readHTMLFormElement, 
    readHTMLHRElement: readHTMLHRElement, 
    readHTMLHeadElement: readHTMLHeadElement, 
    readHTMLHeadingElement: readHTMLHeadingElement, 
    readHTMLHtmlElement: readHTMLHtmlElement, 
    readHTMLIFrameElement: readHTMLIFrameElement, 
    readHTMLImageElement: readHTMLImageElement, 
    readHTMLInputElement: readHTMLInputElement, 
    readHTMLKeygenElement: readHTMLKeygenElement, 
    readHTMLLIElement: readHTMLLIElement, 
    readHTMLLabelElement: readHTMLLabelElement, 
    readHTMLLegendElement: readHTMLLegendElement, 
    readHTMLLinkElement: readHTMLLinkElement, 
    readHTMLMapElement: readHTMLMapElement, 
    readHTMLMediaElement: readHTMLMediaElement, 
    readHTMLMetaElement: readHTMLMetaElement, 
    readHTMLMeterElement: readHTMLMeterElement, 
    readHTMLModElement: readHTMLModElement, 
    readHTMLOListElement: readHTMLOListElement, 
    readHTMLObjectElement: readHTMLObjectElement, 
    readHTMLOptGroupElement: readHTMLOptGroupElement, 
    readHTMLOptionElement: readHTMLOptionElement, 
    readHTMLOutputElement: readHTMLOutputElement, 
    readHTMLParagraphElement: readHTMLParagraphElement, 
    readHTMLParamElement: readHTMLParamElement, 
    readHTMLPreElement: readHTMLPreElement, 
    readHTMLProgressElement: readHTMLProgressElement, 
    readHTMLQuoteElement: readHTMLQuoteElement, 
    readHTMLScriptElement: readHTMLScriptElement, 
    readHTMLSelectElement: readHTMLSelectElement, 
    readHTMLSourceElement: readHTMLSourceElement, 
    readHTMLSpanElement: readHTMLSpanElement, 
    readHTMLStyleElement: readHTMLStyleElement, 
    readHTMLTableCaptionElement: readHTMLTableCaptionElement, 
    readHTMLTableCellElement: readHTMLTableCellElement, 
    readHTMLTableColElement: readHTMLTableColElement, 
    readHTMLTableDataCellElement: readHTMLTableDataCellElement, 
    readHTMLTableElement: readHTMLTableElement, 
    readHTMLTableHeaderCellElement: readHTMLTableHeaderCellElement, 
    readHTMLTableRowElement: readHTMLTableRowElement, 
    readHTMLTableSectionElement: readHTMLTableSectionElement, 
    readHTMLTemplateElement: readHTMLTemplateElement, 
    readHTMLTextAreaElement: readHTMLTextAreaElement, 
    readHTMLTimeElement: readHTMLTimeElement, 
    readHTMLTitleElement: readHTMLTitleElement, 
    readHTMLTrackElement: readHTMLTrackElement, 
    readHTMLUListElement: readHTMLUListElement, 
    readHTMLVideoElement: readHTMLVideoElement, 
    windowToEventTarget: windowToEventTarget, 
    isForeignHTMLDocument: isForeignHTMLDocument, 
    isForeignHTMLElement: isForeignHTMLElement, 
    isForeignHTMLHtmlElement: isForeignHTMLHtmlElement, 
    isForeignHTMLHeadElement: isForeignHTMLHeadElement, 
    isForeignHTMLTitleElement: isForeignHTMLTitleElement, 
    isForeignHTMLBaseElement: isForeignHTMLBaseElement, 
    isForeignHTMLLinkElement: isForeignHTMLLinkElement, 
    isForeignHTMLMetaElement: isForeignHTMLMetaElement, 
    isForeignHTMLStyleElement: isForeignHTMLStyleElement, 
    isForeignHTMLBodyElement: isForeignHTMLBodyElement, 
    isForeignHTMLHeadingElement: isForeignHTMLHeadingElement, 
    isForeignHTMLParagraphElement: isForeignHTMLParagraphElement, 
    isForeignHTMLHRElement: isForeignHTMLHRElement, 
    isForeignHTMLPreElement: isForeignHTMLPreElement, 
    isForeignHTMLQuoteElement: isForeignHTMLQuoteElement, 
    isForeignHTMLOListElement: isForeignHTMLOListElement, 
    isForeignHTMLUListElement: isForeignHTMLUListElement, 
    isForeignHTMLLIElement: isForeignHTMLLIElement, 
    isForeignHTMLDListElement: isForeignHTMLDListElement, 
    isForeignHTMLDivElement: isForeignHTMLDivElement, 
    isForeignHTMLAnchorElement: isForeignHTMLAnchorElement, 
    isForeignHTMLDataElement: isForeignHTMLDataElement, 
    isForeignHTMLTimeElement: isForeignHTMLTimeElement, 
    isForeignHTMLSpanElement: isForeignHTMLSpanElement, 
    isForeignHTMLBRElement: isForeignHTMLBRElement, 
    isForeignHTMLModElement: isForeignHTMLModElement, 
    isForeignHTMLImageElement: isForeignHTMLImageElement, 
    isForeignHTMLIFrameElement: isForeignHTMLIFrameElement, 
    isForeignHTMLEmbedElement: isForeignHTMLEmbedElement, 
    isForeignHTMLObjectElement: isForeignHTMLObjectElement, 
    isForeignHTMLParamElement: isForeignHTMLParamElement, 
    isForeignHTMLSourceElement: isForeignHTMLSourceElement, 
    isForeignHTMLTrackElement: isForeignHTMLTrackElement, 
    isForeignHTMLMapElement: isForeignHTMLMapElement, 
    isForeignHTMLAreaElement: isForeignHTMLAreaElement, 
    isForeignHTMLTableElement: isForeignHTMLTableElement, 
    isForeignHTMLTableCaptionElement: isForeignHTMLTableCaptionElement, 
    isForeignHTMLTableColElement: isForeignHTMLTableColElement, 
    isForeignHTMLTableSectionElement: isForeignHTMLTableSectionElement, 
    isForeignHTMLTableRowElement: isForeignHTMLTableRowElement, 
    isForeignHTMLFormElement: isForeignHTMLFormElement, 
    isForeignHTMLLabelElement: isForeignHTMLLabelElement, 
    isForeignHTMLInputElement: isForeignHTMLInputElement, 
    isForeignHTMLButtonElement: isForeignHTMLButtonElement, 
    isForeignHTMLSelectElement: isForeignHTMLSelectElement, 
    isForeignHTMLDataListElement: isForeignHTMLDataListElement, 
    isForeignHTMLOptGroupElement: isForeignHTMLOptGroupElement, 
    isForeignHTMLOptionElement: isForeignHTMLOptionElement, 
    isForeignHTMLTextAreaElement: isForeignHTMLTextAreaElement, 
    isForeignHTMLKeygenElement: isForeignHTMLKeygenElement, 
    isForeignHTMLOutputElement: isForeignHTMLOutputElement, 
    isForeignHTMLProgressElement: isForeignHTMLProgressElement, 
    isForeignHTMLMeterElement: isForeignHTMLMeterElement, 
    isForeignHTMLFieldSetElement: isForeignHTMLFieldSetElement, 
    isForeignHTMLLegendElement: isForeignHTMLLegendElement, 
    isForeignHTMLScriptElement: isForeignHTMLScriptElement, 
    isForeignHTMLTemplateElement: isForeignHTMLTemplateElement, 
    isForeignHTMLCanvasElement: isForeignHTMLCanvasElement
};

},{"../Control.Semigroupoid":53,"../DOM.Event.Types":55,"../DOM.Node.Types":64,"../Data.Either":77,"../Data.Foreign":94,"../Data.Foreign.Class":85,"../Prelude":162,"../Unsafe.Coerce":165,"./foreign":56}],58:[function(require,module,exports){
"use strict";

exports.document = function (window) {
  return function () {
    return window.document;
  };
};

exports.navigator = function (window) {
  return function () {
    return window.navigator;
  };
};

exports.location = function (window) {
  return function () {
    return window.location;
  };
};

exports.innerWidth = function (window) {
  return function () {
    return window.innerWidth;
  };
};

exports.innerHeight = function (window) {
  return function () {
    return window.innerHeight;
  };
};

},{}],59:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var DOM = require("../DOM");
var DOM_HTML_Types = require("../DOM.HTML.Types");
module.exports = {
    document: $foreign.document, 
    innerHeight: $foreign.innerHeight, 
    innerWidth: $foreign.innerWidth, 
    location: $foreign.location, 
    navigator: $foreign.navigator
};

},{"../Control.Monad.Eff":34,"../DOM":65,"../DOM.HTML.Types":57,"./foreign":58}],60:[function(require,module,exports){
/* global window */
"use strict";

exports.window = function () {
  return window;
};

},{}],61:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var DOM = require("../DOM");
var DOM_HTML_Types = require("../DOM.HTML.Types");
module.exports = {
    window: $foreign.window
};

},{"../Control.Monad.Eff":34,"../DOM":65,"../DOM.HTML.Types":57,"./foreign":60}],62:[function(require,module,exports){
"use strict";

exports.getElementById = function (id) {
  return function (node) {
    return function () {
      return node.getElementById(id);
    };
  };
};

},{}],63:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Data_Nullable = require("../Data.Nullable");
var DOM = require("../DOM");
var DOM_Node_Types = require("../DOM.Node.Types");
module.exports = {
    getElementById: $foreign.getElementById
};

},{"../Control.Monad.Eff":34,"../DOM":65,"../DOM.Node.Types":64,"../Data.Nullable":123,"./foreign":62}],64:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var DOM_Event_Types = require("../DOM.Event.Types");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var ElementId = function (x) {
    return x;
};
var textToNode = Unsafe_Coerce.unsafeCoerce;
var processingInstructionToNode = Unsafe_Coerce.unsafeCoerce;
var elementToParentNode = Unsafe_Coerce.unsafeCoerce;
var elementToNonDocumentTypeChildNode = Unsafe_Coerce.unsafeCoerce;
var elementToNode = Unsafe_Coerce.unsafeCoerce;
var elementToEventTarget = Unsafe_Coerce.unsafeCoerce;
var documentTypeToNode = Unsafe_Coerce.unsafeCoerce;
var documentToParentNode = Unsafe_Coerce.unsafeCoerce;
var documentToNonElementParentNode = Unsafe_Coerce.unsafeCoerce;
var documentToNode = Unsafe_Coerce.unsafeCoerce;
var documentToEventTarget = Unsafe_Coerce.unsafeCoerce;
var documentFragmentToParentNode = Unsafe_Coerce.unsafeCoerce;
var documentFragmentToNonElementParentNode = Unsafe_Coerce.unsafeCoerce;
var documentFragmentToNode = Unsafe_Coerce.unsafeCoerce;
var commentToNode = Unsafe_Coerce.unsafeCoerce;
var characterDataToNonDocumentTypeChildNode = Unsafe_Coerce.unsafeCoerce;
module.exports = {
    ElementId: ElementId, 
    characterDataToNonDocumentTypeChildNode: characterDataToNonDocumentTypeChildNode, 
    commentToNode: commentToNode, 
    documentFragmentToNode: documentFragmentToNode, 
    documentFragmentToNonElementParentNode: documentFragmentToNonElementParentNode, 
    documentFragmentToParentNode: documentFragmentToParentNode, 
    documentToEventTarget: documentToEventTarget, 
    documentToNode: documentToNode, 
    documentToNonElementParentNode: documentToNonElementParentNode, 
    documentToParentNode: documentToParentNode, 
    documentTypeToNode: documentTypeToNode, 
    elementToEventTarget: elementToEventTarget, 
    elementToNode: elementToNode, 
    elementToNonDocumentTypeChildNode: elementToNonDocumentTypeChildNode, 
    elementToParentNode: elementToParentNode, 
    processingInstructionToNode: processingInstructionToNode, 
    textToNode: textToNode
};

},{"../DOM.Event.Types":55,"../Unsafe.Coerce":165}],65:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
module.exports = {};

},{}],66:[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Array

//------------------------------------------------------------------------------
// Array creation --------------------------------------------------------------
//------------------------------------------------------------------------------

exports.range = function (start) {
  return function (end) {
    var step = start > end ? -1 : 1;
    var result = [];
    for (var i = start, n = 0; i !== end; i += step) {
      result[n++] = i;
    }
    result[n] = i;
    return result;
  };
};

exports.fromFoldableImpl = (function () {
  // jshint maxparams: 2
  function Cons(head, tail) {
    this.head = head;
    this.tail = tail;
  }
  var emptyList = {};

  function curryCons(head) {
    return function (tail) {
      return new Cons(head, tail);
    };
  }

  function listToArray(list) {
    var result = [];
    var count = 0;
    while (list !== emptyList) {
      result[count++] = list.head;
      list = list.tail;
    }
    return result;
  }

  return function (foldr) {
    return function (xs) {
      return listToArray(foldr(curryCons)(emptyList)(xs));
    };
  };
})();

//------------------------------------------------------------------------------
// Array size ------------------------------------------------------------------
//------------------------------------------------------------------------------

exports.length = function (xs) {
  return xs.length;
};

//------------------------------------------------------------------------------
// Extending arrays ------------------------------------------------------------
//------------------------------------------------------------------------------

exports.cons = function (e) {
  return function (l) {
    return [e].concat(l);
  };
};

exports.snoc = function (l) {
  return function (e) {
    var l1 = l.slice();
    l1.push(e);
    return l1;
  };
};

//------------------------------------------------------------------------------
// Non-indexed reads -----------------------------------------------------------
//------------------------------------------------------------------------------

exports["uncons'"] = function (empty) {
  return function (next) {
    return function (xs) {
      return xs.length === 0 ? empty({}) : next(xs[0])(xs.slice(1));
    };
  };
};

//------------------------------------------------------------------------------
// Indexed operations ----------------------------------------------------------
//------------------------------------------------------------------------------

exports.indexImpl = function (just) {
  return function (nothing) {
    return function (xs) {
      return function (i) {
        return i < 0 || i >= xs.length ? nothing :  just(xs[i]);
      };
    };
  };
};

exports.findIndexImpl = function (just) {
  return function (nothing) {
    return function (f) {
      return function (xs) {
        for (var i = 0, l = xs.length; i < l; i++) {
          if (f(xs[i])) return just(i);
        }
        return nothing;
      };
    };
  };
};

exports.findLastIndexImpl = function (just) {
  return function (nothing) {
    return function (f) {
      return function (xs) {
        for (var i = xs.length - 1; i >= 0; i--) {
          if (f(xs[i])) return just(i);
        }
        return nothing;
      };
    };
  };
};

exports._insertAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (a) {
        return function (l) {
          if (i < 0 || i > l.length) return nothing;
          var l1 = l.slice();
          l1.splice(i, 0, a);
          return just(l1);
        };
      };
    };
  };
};

exports._deleteAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (l) {
        if (i < 0 || i >= l.length) return nothing;
        var l1 = l.slice();
        l1.splice(i, 1);
        return just(l1);
      };
    };
  };
};

exports._updateAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (a) {
        return function (l) {
          if (i < 0 || i >= l.length) return nothing;
          var l1 = l.slice();
          l1[i] = a;
          return just(l1);
        };
      };
    };
  };
};

//------------------------------------------------------------------------------
// Transformations -------------------------------------------------------------
//------------------------------------------------------------------------------

exports.reverse = function (l) {
  return l.slice().reverse();
};

exports.concat = function (xss) {
  var result = [];
  for (var i = 0, l = xss.length; i < l; i++) {
    var xs = xss[i];
    for (var j = 0, m = xs.length; j < m; j++) {
      result.push(xs[j]);
    }
  }
  return result;
};

exports.filter = function (f) {
  return function (xs) {
    return xs.filter(f);
  };
};

exports.partition = function (f) {
  return function (xs) {
    var yes = [];
    var no  = [];
    for (var i = 0; i < xs.length; i++) {
      var x = xs[i];
      if (f(x))
        yes.push(x);
      else
        no.push(x);
    }
    return { yes: yes, no: no };
  };
};

//------------------------------------------------------------------------------
// Sorting ---------------------------------------------------------------------
//------------------------------------------------------------------------------

exports.sortImpl = function (f) {
  return function (l) {
    // jshint maxparams: 2
    return l.slice().sort(function (x, y) {
      return f(x)(y);
    });
  };
};

//------------------------------------------------------------------------------
// Subarrays -------------------------------------------------------------------
//------------------------------------------------------------------------------

exports.slice = function (s) {
  return function (e) {
    return function (l) {
      return l.slice(s, e);
    };
  };
};

exports.take = function (n) {
  return function (l) {
    return n < 1 ? [] : l.slice(0, n);
  };
};

exports.drop = function (n) {
  return function (l) {
    return n < 1 ? l : l.slice(n);
  };
};

//------------------------------------------------------------------------------
// Zipping ---------------------------------------------------------------------
//------------------------------------------------------------------------------

exports.zipWith = function (f) {
  return function (xs) {
    return function (ys) {
      var l = xs.length < ys.length ? xs.length : ys.length;
      var result = new Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(xs[i])(ys[i]);
      }
      return result;
    };
  };
};

},{}],67:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Lazy = require("../Control.Lazy");
var Data_Foldable = require("../Data.Foldable");
var Data_Maybe = require("../Data.Maybe");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Data_Unfoldable = require("../Data.Unfoldable");
var Partial_Unsafe = require("../Partial.Unsafe");
var Data_Function = require("../Data.Function");
var Data_Ordering = require("../Data.Ordering");
var Data_Ring = require("../Data.Ring");
var Data_Ord = require("../Data.Ord");
var Data_Eq = require("../Data.Eq");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Control_Apply = require("../Control.Apply");
var Data_Functor = require("../Data.Functor");
var Control_Applicative = require("../Control.Applicative");
var Data_Boolean = require("../Data.Boolean");
var Data_Semiring = require("../Data.Semiring");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Control_Bind = require("../Control.Bind");
var Data_Semigroup = require("../Data.Semigroup");
var Control_Category = require("../Control.Category");
var zipWithA = function (dictApplicative) {
    return function (f) {
        return function (xs) {
            return function (ys) {
                return Data_Traversable.sequence(Data_Traversable.traversableArray)(dictApplicative)($foreign.zipWith(f)(xs)(ys));
            };
        };
    };
};
var zip = $foreign.zipWith(Data_Tuple.Tuple.create);
var updateAt = $foreign._updateAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var unzip = $foreign["uncons'"](function (v) {
    return new Data_Tuple.Tuple([  ], [  ]);
})(function (v) {
    return function (ts) {
        var $39 = unzip(ts);
        return new Data_Tuple.Tuple($foreign.cons(v.value0)($39.value0), $foreign.cons(v.value1)($39.value1));
    };
});
var uncons = $foreign["uncons'"](Data_Function["const"](Data_Maybe.Nothing.value))(function (x) {
    return function (xs) {
        return new Data_Maybe.Just({
            head: x, 
            tail: xs
        });
    };
});
var toUnfoldable = function (dictUnfoldable) {
    return Data_Function.apply(Data_Unfoldable.unfoldr(dictUnfoldable))($foreign["uncons'"](Data_Function["const"](Data_Maybe.Nothing.value))(function (h) {
        return function (t) {
            return new Data_Maybe.Just(new Data_Tuple.Tuple(h, t));
        };
    }));
};
var tail = $foreign["uncons'"](Data_Function["const"](Data_Maybe.Nothing.value))(function (v) {
    return function (xs) {
        return new Data_Maybe.Just(xs);
    };
});
var span = function (p) {
    var go = function (__copy_acc) {
        return function (__copy_xs) {
            var acc = __copy_acc;
            var xs = __copy_xs;
            tco: while (true) {
                var $45 = uncons(xs);
                if ($45 instanceof Data_Maybe.Just && p($45.value0.head)) {
                    var __tco_acc = $foreign.cons($45.value0.head)(acc);
                    acc = __tco_acc;
                    xs = $45.value0.tail;
                    continue tco;
                };
                return {
                    init: $foreign.reverse(acc), 
                    rest: xs
                };
            };
        };
    };
    return go([  ]);
};
var takeWhile = function (p) {
    return function (xs) {
        return (span(p)(xs)).init;
    };
};
var sortBy = function (comp) {
    return function (xs) {
        var comp$prime = function (x) {
            return function (y) {
                var $49 = comp(x)(y);
                if ($49 instanceof Data_Ordering.GT) {
                    return 1;
                };
                if ($49 instanceof Data_Ordering.EQ) {
                    return 0;
                };
                if ($49 instanceof Data_Ordering.LT) {
                    return -1;
                };
                throw new Error("Failed pattern match at Data.Array line 436, column 15 - line 441, column 1: " + [ $49.constructor.name ]);
            };
        };
        return $foreign.sortImpl(comp$prime)(xs);
    };
};
var sort = function (dictOrd) {
    return function (xs) {
        return sortBy(Data_Ord.compare(dictOrd))(xs);
    };
};
var singleton = function (a) {
    return [ a ];
};
var $$null = function (xs) {
    return $foreign.length(xs) === 0;
};
var nubBy = function (eq) {
    return function (xs) {
        var $50 = uncons(xs);
        if ($50 instanceof Data_Maybe.Just) {
            return $foreign.cons($50.value0.head)(nubBy(eq)($foreign.filter(function (y) {
                return !eq($50.value0.head)(y);
            })($50.value0.tail)));
        };
        if ($50 instanceof Data_Maybe.Nothing) {
            return [  ];
        };
        throw new Error("Failed pattern match at Data.Array line 525, column 3 - line 527, column 18: " + [ $50.constructor.name ]);
    };
};
var nub = function (dictEq) {
    return nubBy(Data_Eq.eq(dictEq));
};
var mapWithIndex = function (f) {
    return function (xs) {
        return $foreign.zipWith(f)($foreign.range(0)($foreign.length(xs) - 1))(xs);
    };
};
var some = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Control_Apply.apply((dictAlternative["__superclass_Control.Applicative.Applicative_0"]())["__superclass_Control.Apply.Apply_0"]())(Data_Functor.map(((dictAlternative["__superclass_Control.Plus.Plus_1"]())["__superclass_Control.Alt.Alt_0"]())["__superclass_Data.Functor.Functor_0"]())($foreign.cons)(v))(Control_Lazy.defer(dictLazy)(function (v1) {
                return many(dictAlternative)(dictLazy)(v);
            }));
        };
    };
};
var many = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Control_Alt.alt((dictAlternative["__superclass_Control.Plus.Plus_1"]())["__superclass_Control.Alt.Alt_0"]())(some(dictAlternative)(dictLazy)(v))(Control_Applicative.pure(dictAlternative["__superclass_Control.Applicative.Applicative_0"]())([  ]));
        };
    };
};
var insertAt = $foreign._insertAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var init = function (xs) {
    if ($$null(xs)) {
        return Data_Maybe.Nothing.value;
    };
    if (Data_Boolean.otherwise) {
        return new Data_Maybe.Just($foreign.slice(0)($foreign.length(xs) - 1)(xs));
    };
    throw new Error("Failed pattern match at Data.Array line 227, column 1 - line 229, column 55: " + [ xs.constructor.name ]);
};
var index = $foreign.indexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var last = function (xs) {
    return index(xs)($foreign.length(xs) - 1);
};
var modifyAt = function (i) {
    return function (f) {
        return function (xs) {
            var go = function (x) {
                return updateAt(i)(f(x))(xs);
            };
            return Data_Maybe.maybe(Data_Maybe.Nothing.value)(go)(index(xs)(i));
        };
    };
};
var head = $foreign["uncons'"](Data_Function["const"](Data_Maybe.Nothing.value))(function (x) {
    return function (v) {
        return new Data_Maybe.Just(x);
    };
});
var groupBy = function (op) {
    var go = function (__copy_acc) {
        return function (__copy_xs) {
            var acc = __copy_acc;
            var xs = __copy_xs;
            tco: while (true) {
                var $55 = uncons(xs);
                if ($55 instanceof Data_Maybe.Just) {
                    var sp = span(op($55.value0.head))($55.value0.tail);
                    var __tco_acc = $foreign.cons($foreign.cons($55.value0.head)(sp.init))(acc);
                    acc = __tco_acc;
                    xs = sp.rest;
                    continue tco;
                };
                if ($55 instanceof Data_Maybe.Nothing) {
                    return $foreign.reverse(acc);
                };
                throw new Error("Failed pattern match at Data.Array line 511, column 15 - line 515, column 27: " + [ $55.constructor.name ]);
            };
        };
    };
    return go([  ]);
};
var group = function (dictEq) {
    return function (xs) {
        return groupBy(Data_Eq.eq(dictEq))(xs);
    };
};
var group$prime = function (dictOrd) {
    return function ($69) {
        return group(dictOrd["__superclass_Data.Eq.Eq_0"]())(sort(dictOrd)($69));
    };
};
var fromFoldable = function (dictFoldable) {
    return $foreign.fromFoldableImpl(Data_Foldable.foldr(dictFoldable));
};
var foldM = function (dictMonad) {
    return function (f) {
        return function (a) {
            return $foreign["uncons'"](function (v) {
                return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())(a);
            })(function (b) {
                return function (bs) {
                    return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(f(a)(b))(function (a$prime) {
                        return foldM(dictMonad)(f)(a$prime)(bs);
                    });
                };
            });
        };
    };
};
var findLastIndex = $foreign.findLastIndexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var insertBy = function (cmp) {
    return function (x) {
        return function (ys) {
            var i = Data_Maybe.maybe(0)(function (v) {
                return v + 1 | 0;
            })(findLastIndex(function (y) {
                return Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(x)(y))(Data_Ordering.GT.value);
            })(ys));
            return Partial_Unsafe.unsafePartial(function (dictPartial) {
                return Data_Maybe.fromJust(dictPartial)(insertAt(i)(x)(ys));
            });
        };
    };
};
var insert = function (dictOrd) {
    return insertBy(Data_Ord.compare(dictOrd));
};
var findIndex = $foreign.findIndexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var intersectBy = function (eq) {
    return function (xs) {
        return function (ys) {
            return $foreign.filter(function (x) {
                return Data_Maybe.isJust(findIndex(eq(x))(ys));
            })(xs);
        };
    };
};
var intersect = function (dictEq) {
    return intersectBy(Data_Eq.eq(dictEq));
};
var filterM = function (dictMonad) {
    return function (p) {
        return $foreign["uncons'"](function (v) {
            return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())([  ]);
        })(function (x) {
            return function (xs) {
                return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(p(x))(function (v) {
                    return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(filterM(dictMonad)(p)(xs))(function (v1) {
                        return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())((function () {
                            if (v) {
                                return $foreign.cons(x)(v1);
                            };
                            if (!v) {
                                return v1;
                            };
                            throw new Error("Failed pattern match at Data.Array line 403, column 3 - line 403, column 34: " + [ v.constructor.name ]);
                        })());
                    });
                });
            };
        });
    };
};
var elemLastIndex = function (dictEq) {
    return function (x) {
        return findLastIndex(function (v) {
            return Data_Eq.eq(dictEq)(v)(x);
        });
    };
};
var elemIndex = function (dictEq) {
    return function (x) {
        return findIndex(function (v) {
            return Data_Eq.eq(dictEq)(v)(x);
        });
    };
};
var dropWhile = function (p) {
    return function (xs) {
        return (span(p)(xs)).rest;
    };
};
var deleteAt = $foreign._deleteAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var deleteBy = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2.length === 0) {
                return [  ];
            };
            return Data_Maybe.maybe(v2)(function (i) {
                return Data_Function.apply(Partial_Unsafe.unsafePartial)(function (dictPartial) {
                    return Data_Maybe.fromJust(dictPartial)(deleteAt(i)(v2));
                });
            })(findIndex(v(v1))(v2));
        };
    };
};
var unionBy = function (eq) {
    return function (xs) {
        return function (ys) {
            return Data_Semigroup.append(Data_Semigroup.semigroupArray)(xs)(Data_Foldable.foldl(Data_Foldable.foldableArray)(Data_Function.flip(deleteBy(eq)))(nubBy(eq)(ys))(xs));
        };
    };
};
var union = function (dictEq) {
    return unionBy(Data_Eq.eq(dictEq));
};
var $$delete = function (dictEq) {
    return deleteBy(Data_Eq.eq(dictEq));
};
var difference = function (dictEq) {
    return function (xs) {
        return function (ys) {
            if ($$null(xs)) {
                return [  ];
            };
            if (Data_Boolean.otherwise) {
                return $foreign["uncons'"](Data_Function["const"](xs))(function (z) {
                    return function (zs) {
                        return difference(dictEq)($$delete(dictEq)(z)(xs))(zs);
                    };
                })(ys);
            };
            throw new Error("Failed pattern match at Data.Array line 557, column 1 - line 559, column 67: " + [ xs.constructor.name, ys.constructor.name ]);
        };
    };
};
var concatMap = Data_Function.flip(Control_Bind.bind(Control_Bind.bindArray));
var mapMaybe = function (f) {
    return concatMap(function ($70) {
        return Data_Maybe.maybe([  ])(singleton)(f($70));
    });
};
var catMaybes = mapMaybe(Control_Category.id(Control_Category.categoryFn));
var alterAt = function (i) {
    return function (f) {
        return function (xs) {
            var go = function (x) {
                var $67 = f(x);
                if ($67 instanceof Data_Maybe.Nothing) {
                    return deleteAt(i)(xs);
                };
                if ($67 instanceof Data_Maybe.Just) {
                    return updateAt(i)($67.value0)(xs);
                };
                throw new Error("Failed pattern match at Data.Array line 361, column 10 - line 363, column 32: " + [ $67.constructor.name ]);
            };
            return Data_Maybe.maybe(Data_Maybe.Nothing.value)(go)(index(xs)(i));
        };
    };
};
module.exports = {
    alterAt: alterAt, 
    catMaybes: catMaybes, 
    concatMap: concatMap, 
    "delete": $$delete, 
    deleteAt: deleteAt, 
    deleteBy: deleteBy, 
    difference: difference, 
    dropWhile: dropWhile, 
    elemIndex: elemIndex, 
    elemLastIndex: elemLastIndex, 
    filterM: filterM, 
    findIndex: findIndex, 
    findLastIndex: findLastIndex, 
    foldM: foldM, 
    fromFoldable: fromFoldable, 
    group: group, 
    "group'": group$prime, 
    groupBy: groupBy, 
    head: head, 
    index: index, 
    init: init, 
    insert: insert, 
    insertAt: insertAt, 
    insertBy: insertBy, 
    intersect: intersect, 
    intersectBy: intersectBy, 
    last: last, 
    many: many, 
    mapMaybe: mapMaybe, 
    mapWithIndex: mapWithIndex, 
    modifyAt: modifyAt, 
    nub: nub, 
    nubBy: nubBy, 
    "null": $$null, 
    singleton: singleton, 
    some: some, 
    sort: sort, 
    sortBy: sortBy, 
    span: span, 
    tail: tail, 
    takeWhile: takeWhile, 
    toUnfoldable: toUnfoldable, 
    uncons: uncons, 
    union: union, 
    unionBy: unionBy, 
    unzip: unzip, 
    updateAt: updateAt, 
    zip: zip, 
    zipWithA: zipWithA, 
    concat: $foreign.concat, 
    cons: $foreign.cons, 
    drop: $foreign.drop, 
    filter: $foreign.filter, 
    length: $foreign.length, 
    partition: $foreign.partition, 
    range: $foreign.range, 
    reverse: $foreign.reverse, 
    slice: $foreign.slice, 
    snoc: $foreign.snoc, 
    take: $foreign.take, 
    zipWith: $foreign.zipWith
};

},{"../Control.Alt":5,"../Control.Alternative":6,"../Control.Applicative":7,"../Control.Apply":9,"../Control.Bind":13,"../Control.Category":14,"../Control.Lazy":17,"../Control.Semigroupoid":53,"../Data.Boolean":71,"../Data.Eq":79,"../Data.Foldable":84,"../Data.Function":97,"../Data.Functor":100,"../Data.HeytingAlgebra":104,"../Data.Maybe":113,"../Data.Ord":127,"../Data.Ordering":128,"../Data.Ring":130,"../Data.Semigroup":132,"../Data.Semiring":134,"../Data.Traversable":146,"../Data.Tuple":147,"../Data.Unfoldable":149,"../Partial.Unsafe":159,"../Prelude":162,"./foreign":66}],68:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Data_BooleanAlgebra = require("../Data.BooleanAlgebra");
var Data_Function = require("../Data.Function");
var Data_Monoid = require("../Data.Monoid");
var Data_Monoid_Conj = require("../Data.Monoid.Conj");
var Data_Monoid_Disj = require("../Data.Monoid.Disj");
var Data_Monoid_Dual = require("../Data.Monoid.Dual");
var Data_Monoid_Endo = require("../Data.Monoid.Endo");
var Data_Unit = require("../Data.Unit");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Control_Category = require("../Control.Category");
var Data_Semigroup = require("../Data.Semigroup");
var Bifoldable = function (bifoldMap, bifoldl, bifoldr) {
    this.bifoldMap = bifoldMap;
    this.bifoldl = bifoldl;
    this.bifoldr = bifoldr;
};
var bifoldr = function (dict) {
    return dict.bifoldr;
};
var bitraverse_ = function (dictBifoldable) {
    return function (dictApplicative) {
        return function (f) {
            return function (g) {
                return bifoldr(dictBifoldable)(function ($18) {
                    return Control_Apply.applySecond(dictApplicative["__superclass_Control.Apply.Apply_0"]())(f($18));
                })(function ($19) {
                    return Control_Apply.applySecond(dictApplicative["__superclass_Control.Apply.Apply_0"]())(g($19));
                })(Control_Applicative.pure(dictApplicative)(Data_Unit.unit));
            };
        };
    };
};
var bifor_ = function (dictBifoldable) {
    return function (dictApplicative) {
        return function (t) {
            return function (f) {
                return function (g) {
                    return bitraverse_(dictBifoldable)(dictApplicative)(f)(g)(t);
                };
            };
        };
    };
};
var bisequence_ = function (dictBifoldable) {
    return function (dictApplicative) {
        return bitraverse_(dictBifoldable)(dictApplicative)(Control_Category.id(Control_Category.categoryFn))(Control_Category.id(Control_Category.categoryFn));
    };
};
var bifoldl = function (dict) {
    return dict.bifoldl;
};
var bifoldMapDefaultR = function (dictBifoldable) {
    return function (dictMonoid) {
        return function (f) {
            return function (g) {
                return function (p) {
                    return bifoldr(dictBifoldable)(function ($20) {
                        return Data_Semigroup.append(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]())(f($20));
                    })(function ($21) {
                        return Data_Semigroup.append(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]())(g($21));
                    })(Data_Monoid.mempty(dictMonoid))(p);
                };
            };
        };
    };
};
var bifoldMapDefaultL = function (dictBifoldable) {
    return function (dictMonoid) {
        return function (f) {
            return function (g) {
                return function (p) {
                    return bifoldl(dictBifoldable)(function (m) {
                        return function (a) {
                            return Data_Semigroup.append(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]())(m)(f(a));
                        };
                    })(function (m) {
                        return function (b) {
                            return Data_Semigroup.append(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]())(m)(g(b));
                        };
                    })(Data_Monoid.mempty(dictMonoid))(p);
                };
            };
        };
    };
};
var bifoldMap = function (dict) {
    return dict.bifoldMap;
};
var bifoldlDefault = function (dictBifoldable) {
    return function (f) {
        return function (g) {
            return function (z) {
                return function (p) {
                    return Data_Monoid_Endo.runEndo(Data_Monoid_Dual.runDual(bifoldMap(dictBifoldable)(Data_Monoid_Dual.monoidDual(Data_Monoid_Endo.monoidEndo))(function ($22) {
                        return Data_Monoid_Dual.Dual(Data_Monoid_Endo.Endo(Data_Function.flip(f)($22)));
                    })(function ($23) {
                        return Data_Monoid_Dual.Dual(Data_Monoid_Endo.Endo(Data_Function.flip(g)($23)));
                    })(p)))(z);
                };
            };
        };
    };
};
var bifoldrDefault = function (dictBifoldable) {
    return function (f) {
        return function (g) {
            return function (z) {
                return function (p) {
                    return Data_Monoid_Endo.runEndo(bifoldMap(dictBifoldable)(Data_Monoid_Endo.monoidEndo)(function ($24) {
                        return Data_Monoid_Endo.Endo(f($24));
                    })(function ($25) {
                        return Data_Monoid_Endo.Endo(g($25));
                    })(p))(z);
                };
            };
        };
    };
};
var bifold = function (dictBifoldable) {
    return function (dictMonoid) {
        return bifoldMap(dictBifoldable)(dictMonoid)(Control_Category.id(Control_Category.categoryFn))(Control_Category.id(Control_Category.categoryFn));
    };
};
var biany = function (dictBifoldable) {
    return function (dictBooleanAlgebra) {
        return function (p) {
            return function (q) {
                return function ($26) {
                    return Data_Monoid_Disj.runDisj(bifoldMap(dictBifoldable)(Data_Monoid_Disj.monoidDisj(dictBooleanAlgebra["__superclass_Data.HeytingAlgebra.HeytingAlgebra_0"]()))(function ($27) {
                        return Data_Monoid_Disj.Disj(p($27));
                    })(function ($28) {
                        return Data_Monoid_Disj.Disj(q($28));
                    })($26));
                };
            };
        };
    };
};
var biall = function (dictBifoldable) {
    return function (dictBooleanAlgebra) {
        return function (p) {
            return function (q) {
                return function ($29) {
                    return Data_Monoid_Conj.runConj(bifoldMap(dictBifoldable)(Data_Monoid_Conj.monoidConj(dictBooleanAlgebra["__superclass_Data.HeytingAlgebra.HeytingAlgebra_0"]()))(function ($30) {
                        return Data_Monoid_Conj.Conj(p($30));
                    })(function ($31) {
                        return Data_Monoid_Conj.Conj(q($31));
                    })($29));
                };
            };
        };
    };
};
module.exports = {
    Bifoldable: Bifoldable, 
    biall: biall, 
    biany: biany, 
    bifold: bifold, 
    bifoldMap: bifoldMap, 
    bifoldMapDefaultL: bifoldMapDefaultL, 
    bifoldMapDefaultR: bifoldMapDefaultR, 
    bifoldl: bifoldl, 
    bifoldlDefault: bifoldlDefault, 
    bifoldr: bifoldr, 
    bifoldrDefault: bifoldrDefault, 
    bifor_: bifor_, 
    bisequence_: bisequence_, 
    bitraverse_: bitraverse_
};

},{"../Control.Applicative":7,"../Control.Apply":9,"../Control.Category":14,"../Control.Semigroupoid":53,"../Data.BooleanAlgebra":72,"../Data.Function":97,"../Data.Monoid":120,"../Data.Monoid.Conj":115,"../Data.Monoid.Disj":116,"../Data.Monoid.Dual":117,"../Data.Monoid.Endo":118,"../Data.Semigroup":132,"../Data.Unit":151}],69:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Category = require("../Control.Category");
var Bifunctor = function (bimap) {
    this.bimap = bimap;
};
var bimap = function (dict) {
    return dict.bimap;
};
var lmap = function (dictBifunctor) {
    return function (f) {
        return bimap(dictBifunctor)(f)(Control_Category.id(Control_Category.categoryFn));
    };
};
var rmap = function (dictBifunctor) {
    return bimap(dictBifunctor)(Control_Category.id(Control_Category.categoryFn));
};
module.exports = {
    Bifunctor: Bifunctor, 
    bimap: bimap, 
    lmap: lmap, 
    rmap: rmap
};

},{"../Control.Category":14}],70:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Category = require("../Control.Category");
var Data_Bifoldable = require("../Data.Bifoldable");
var Data_Bifunctor = require("../Data.Bifunctor");
var Bitraversable = function (__superclass_Data$dotBifoldable$dotBifoldable_1, __superclass_Data$dotBifunctor$dotBifunctor_0, bisequence, bitraverse) {
    this["__superclass_Data.Bifoldable.Bifoldable_1"] = __superclass_Data$dotBifoldable$dotBifoldable_1;
    this["__superclass_Data.Bifunctor.Bifunctor_0"] = __superclass_Data$dotBifunctor$dotBifunctor_0;
    this.bisequence = bisequence;
    this.bitraverse = bitraverse;
};
var bitraverse = function (dict) {
    return dict.bitraverse;
};
var bisequenceDefault = function (dictBitraversable) {
    return function (dictApplicative) {
        return function (t) {
            return bitraverse(dictBitraversable)(dictApplicative)(Control_Category.id(Control_Category.categoryFn))(Control_Category.id(Control_Category.categoryFn))(t);
        };
    };
};
var bisequence = function (dict) {
    return dict.bisequence;
};
var bitraverseDefault = function (dictBitraversable) {
    return function (dictApplicative) {
        return function (f) {
            return function (g) {
                return function (t) {
                    return bisequence(dictBitraversable)(dictApplicative)(Data_Bifunctor.bimap(dictBitraversable["__superclass_Data.Bifunctor.Bifunctor_0"]())(f)(g)(t));
                };
            };
        };
    };
};
var bifor = function (dictBitraversable) {
    return function (dictApplicative) {
        return function (t) {
            return function (f) {
                return function (g) {
                    return bitraverse(dictBitraversable)(dictApplicative)(f)(g)(t);
                };
            };
        };
    };
};
module.exports = {
    Bitraversable: Bitraversable, 
    bifor: bifor, 
    bisequence: bisequence, 
    bisequenceDefault: bisequenceDefault, 
    bitraverse: bitraverse, 
    bitraverseDefault: bitraverseDefault
};

},{"../Control.Applicative":7,"../Control.Category":14,"../Data.Bifoldable":68,"../Data.Bifunctor":69}],71:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var otherwise = true;
module.exports = {
    otherwise: otherwise
};

},{}],72:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Unit = require("../Data.Unit");
var BooleanAlgebra = function (__superclass_Data$dotHeytingAlgebra$dotHeytingAlgebra_0) {
    this["__superclass_Data.HeytingAlgebra.HeytingAlgebra_0"] = __superclass_Data$dotHeytingAlgebra$dotHeytingAlgebra_0;
};
var booleanAlgebraUnit = new BooleanAlgebra(function () {
    return Data_HeytingAlgebra.heytingAlgebraUnit;
});
var booleanAlgebraBoolean = new BooleanAlgebra(function () {
    return Data_HeytingAlgebra.heytingAlgebraBoolean;
});
module.exports = {
    BooleanAlgebra: BooleanAlgebra, 
    booleanAlgebraBoolean: booleanAlgebraBoolean, 
    booleanAlgebraUnit: booleanAlgebraUnit
};

},{"../Data.HeytingAlgebra":104,"../Data.Unit":151}],73:[function(require,module,exports){
"use strict";

// module Data.Bounded

exports.topInt = 2147483647;
exports.bottomInt = -2147483648;

exports.topChar = String.fromCharCode(65535);
exports.bottomChar = String.fromCharCode(0);

},{}],74:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Data_Ord = require("../Data.Ord");
var Data_Unit = require("../Data.Unit");
var Data_Ordering = require("../Data.Ordering");
var Bounded = function (__superclass_Data$dotOrd$dotOrd_0, bottom, top) {
    this["__superclass_Data.Ord.Ord_0"] = __superclass_Data$dotOrd$dotOrd_0;
    this.bottom = bottom;
    this.top = top;
};
var top = function (dict) {
    return dict.top;
};
var boundedUnit = new Bounded(function () {
    return Data_Ord.ordUnit;
}, Data_Unit.unit, Data_Unit.unit);
var boundedOrdering = new Bounded(function () {
    return Data_Ord.ordOrdering;
}, Data_Ordering.LT.value, Data_Ordering.GT.value);
var boundedInt = new Bounded(function () {
    return Data_Ord.ordInt;
}, $foreign.bottomInt, $foreign.topInt);
var boundedChar = new Bounded(function () {
    return Data_Ord.ordChar;
}, $foreign.bottomChar, $foreign.topChar);
var boundedBoolean = new Bounded(function () {
    return Data_Ord.ordBoolean;
}, false, true);
var bottom = function (dict) {
    return dict.bottom;
};
module.exports = {
    Bounded: Bounded, 
    bottom: bottom, 
    top: top, 
    boundedBoolean: boundedBoolean, 
    boundedInt: boundedInt, 
    boundedChar: boundedChar, 
    boundedOrdering: boundedOrdering, 
    boundedUnit: boundedUnit
};

},{"../Data.Ord":127,"../Data.Ordering":128,"../Data.Unit":151,"./foreign":73}],75:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Data_Ring = require("../Data.Ring");
var Data_Semiring = require("../Data.Semiring");
var Data_Unit = require("../Data.Unit");
var CommutativeRing = function (__superclass_Data$dotRing$dotRing_0) {
    this["__superclass_Data.Ring.Ring_0"] = __superclass_Data$dotRing$dotRing_0;
};
var commutativeRingUnit = new CommutativeRing(function () {
    return Data_Ring.ringUnit;
});
var commutativeRingNumber = new CommutativeRing(function () {
    return Data_Ring.ringNumber;
});
var commutativeRingInt = new CommutativeRing(function () {
    return Data_Ring.ringInt;
});
module.exports = {
    CommutativeRing: CommutativeRing, 
    commutativeRingInt: commutativeRingInt, 
    commutativeRingNumber: commutativeRingNumber, 
    commutativeRingUnit: commutativeRingUnit
};

},{"../Data.Ring":130,"../Data.Semiring":134,"../Data.Unit":151}],76:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Identity = require("../Data.Identity");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Control_Category = require("../Control.Category");
var Distributive = function (__superclass_Data$dotFunctor$dotFunctor_0, collect, distribute) {
    this["__superclass_Data.Functor.Functor_0"] = __superclass_Data$dotFunctor$dotFunctor_0;
    this.collect = collect;
    this.distribute = distribute;
};
var distributiveIdentity = new Distributive(function () {
    return Data_Identity.functorIdentity;
}, function (dictFunctor) {
    return function (f) {
        return function ($11) {
            return Data_Identity.Identity(Data_Functor.map(dictFunctor)(function ($12) {
                return Data_Identity.runIdentity(f($12));
            })($11));
        };
    };
}, function (dictFunctor) {
    return function ($13) {
        return Data_Identity.Identity(Data_Functor.map(dictFunctor)(Data_Identity.runIdentity)($13));
    };
});
var distribute = function (dict) {
    return dict.distribute;
};
var distributiveFunction = new Distributive(function () {
    return Data_Functor.functorFn;
}, function (dictFunctor) {
    return function (f) {
        return function ($14) {
            return distribute(distributiveFunction)(dictFunctor)(Data_Functor.map(dictFunctor)(f)($14));
        };
    };
}, function (dictFunctor) {
    return function (a) {
        return function (e) {
            return Data_Functor.map(dictFunctor)(function (v) {
                return Data_Function.apply(v)(e);
            })(a);
        };
    };
});
var cotraverse = function (dictDistributive) {
    return function (dictFunctor) {
        return function (f) {
            return function ($15) {
                return Data_Functor.map(dictDistributive["__superclass_Data.Functor.Functor_0"]())(f)(distribute(dictDistributive)(dictFunctor)($15));
            };
        };
    };
};
var collectDefault = function (dictDistributive) {
    return function (dictFunctor) {
        return function (f) {
            return function ($16) {
                return distribute(dictDistributive)(dictFunctor)(Data_Functor.map(dictFunctor)(f)($16));
            };
        };
    };
};
var collect = function (dict) {
    return dict.collect;
};
var distributeDefault = function (dictDistributive) {
    return function (dictFunctor) {
        return collect(dictDistributive)(dictFunctor)(Control_Category.id(Control_Category.categoryFn));
    };
};
module.exports = {
    Distributive: Distributive, 
    collect: collect, 
    collectDefault: collectDefault, 
    cotraverse: cotraverse, 
    distribute: distribute, 
    distributeDefault: distributeDefault, 
    distributiveIdentity: distributiveIdentity, 
    distributiveFunction: distributiveFunction
};

},{"../Control.Category":14,"../Control.Semigroupoid":53,"../Data.Function":97,"../Data.Functor":100,"../Data.Identity":105}],77:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Extend = require("../Control.Extend");
var Control_Monad = require("../Control.Monad");
var Data_Bifoldable = require("../Data.Bifoldable");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Bitraversable = require("../Data.Bitraversable");
var Data_Bounded = require("../Data.Bounded");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Monoid = require("../Data.Monoid");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_Traversable = require("../Data.Traversable");
var Left = (function () {
    function Left(value0) {
        this.value0 = value0;
    };
    Left.create = function (value0) {
        return new Left(value0);
    };
    return Left;
})();
var Right = (function () {
    function Right(value0) {
        this.value0 = value0;
    };
    Right.create = function (value0) {
        return new Right(value0);
    };
    return Right;
})();
var showEither = function (dictShow) {
    return function (dictShow1) {
        return new Data_Show.Show(function (v) {
            if (v instanceof Left) {
                return "(Left " + (Data_Show.show(dictShow)(v.value0) + ")");
            };
            if (v instanceof Right) {
                return "(Right " + (Data_Show.show(dictShow1)(v.value0) + ")");
            };
            throw new Error("Failed pattern match at Data.Either line 171, column 3 - line 172, column 3: " + [ v.constructor.name ]);
        });
    };
};
var functorEither = new Data_Functor.Functor(function (v) {
    return function (v1) {
        if (v1 instanceof Left) {
            return new Left(v1.value0);
        };
        if (v1 instanceof Right) {
            return new Right(v(v1.value0));
        };
        throw new Error("Failed pattern match at Data.Either line 46, column 3 - line 46, column 26: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var invariantEither = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorEither));
var fromRight = function (dictPartial) {
    return function (v) {
        var __unused = function (dictPartial1) {
            return function ($dollar52) {
                return $dollar52;
            };
        };
        return __unused(dictPartial)((function () {
            if (v instanceof Right) {
                return v.value0;
            };
            throw new Error("Failed pattern match at Data.Either line 262, column 1 - line 262, column 23: " + [ v.constructor.name ]);
        })());
    };
};
var fromLeft = function (dictPartial) {
    return function (v) {
        var __unused = function (dictPartial1) {
            return function ($dollar56) {
                return $dollar56;
            };
        };
        return __unused(dictPartial)((function () {
            if (v instanceof Left) {
                return v.value0;
            };
            throw new Error("Failed pattern match at Data.Either line 257, column 1 - line 257, column 22: " + [ v.constructor.name ]);
        })());
    };
};
var foldableEither = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            if (v instanceof Left) {
                return Data_Monoid.mempty(dictMonoid);
            };
            if (v instanceof Right) {
                return f(v.value0);
            };
            throw new Error("Failed pattern match at Data.Either line 202, column 3 - line 202, column 31: " + [ f.constructor.name, v.constructor.name ]);
        };
    };
}, function (v) {
    return function (z) {
        return function (v1) {
            if (v1 instanceof Left) {
                return z;
            };
            if (v1 instanceof Right) {
                return v(z)(v1.value0);
            };
            throw new Error("Failed pattern match at Data.Either line 200, column 3 - line 200, column 26: " + [ v.constructor.name, z.constructor.name, v1.constructor.name ]);
        };
    };
}, function (v) {
    return function (z) {
        return function (v1) {
            if (v1 instanceof Left) {
                return z;
            };
            if (v1 instanceof Right) {
                return v(v1.value0)(z);
            };
            throw new Error("Failed pattern match at Data.Either line 198, column 3 - line 198, column 26: " + [ v.constructor.name, z.constructor.name, v1.constructor.name ]);
        };
    };
});
var traversableEither = new Data_Traversable.Traversable(function () {
    return foldableEither;
}, function () {
    return functorEither;
}, function (dictApplicative) {
    return function (v) {
        if (v instanceof Left) {
            return Control_Applicative.pure(dictApplicative)(new Left(v.value0));
        };
        if (v instanceof Right) {
            return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Right.create)(v.value0);
        };
        throw new Error("Failed pattern match at Data.Either line 216, column 3 - line 216, column 36: " + [ v.constructor.name ]);
    };
}, function (dictApplicative) {
    return function (v) {
        return function (v1) {
            if (v1 instanceof Left) {
                return Control_Applicative.pure(dictApplicative)(new Left(v1.value0));
            };
            if (v1 instanceof Right) {
                return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Right.create)(v(v1.value0));
            };
            throw new Error("Failed pattern match at Data.Either line 214, column 3 - line 214, column 39: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
});
var extendEither = new Control_Extend.Extend(function () {
    return functorEither;
}, function (v) {
    return function (v1) {
        if (v1 instanceof Left) {
            return new Left(v1.value0);
        };
        return new Right(v(v1));
    };
});
var eqEither = function (dictEq) {
    return function (dictEq1) {
        return new Data_Eq.Eq(function (v) {
            return function (v1) {
                if (v instanceof Left && v1 instanceof Left) {
                    return Data_Eq.eq(dictEq)(v.value0)(v1.value0);
                };
                if (v instanceof Right && v1 instanceof Right) {
                    return Data_Eq.eq(dictEq1)(v.value0)(v1.value0);
                };
                return false;
            };
        });
    };
};
var ordEither = function (dictOrd) {
    return function (dictOrd1) {
        return new Data_Ord.Ord(function () {
            return eqEither(dictOrd["__superclass_Data.Eq.Eq_0"]())(dictOrd1["__superclass_Data.Eq.Eq_0"]());
        }, function (v) {
            return function (v1) {
                if (v instanceof Left && v1 instanceof Left) {
                    return Data_Ord.compare(dictOrd)(v.value0)(v1.value0);
                };
                if (v instanceof Right && v1 instanceof Right) {
                    return Data_Ord.compare(dictOrd1)(v.value0)(v1.value0);
                };
                if (v instanceof Left) {
                    return Data_Ordering.LT.value;
                };
                if (v1 instanceof Left) {
                    return Data_Ordering.GT.value;
                };
                throw new Error("Failed pattern match at Data.Either line 188, column 3 - line 188, column 48: " + [ v.constructor.name, v1.constructor.name ]);
            };
        });
    };
};
var either = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Left) {
                return v(v2.value0);
            };
            if (v2 instanceof Right) {
                return v1(v2.value0);
            };
            throw new Error("Failed pattern match at Data.Either line 243, column 1 - line 243, column 26: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
};
var isLeft = either(Data_Function["const"](true))(Data_Function["const"](false));
var isRight = either(Data_Function["const"](false))(Data_Function["const"](true));
var boundedEither = function (dictBounded) {
    return function (dictBounded1) {
        return new Data_Bounded.Bounded(function () {
            return ordEither(dictBounded["__superclass_Data.Ord.Ord_0"]())(dictBounded1["__superclass_Data.Ord.Ord_0"]());
        }, new Left(Data_Bounded.bottom(dictBounded)), new Right(Data_Bounded.top(dictBounded1)));
    };
};
var bifunctorEither = new Data_Bifunctor.Bifunctor(function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Left) {
                return new Left(v(v2.value0));
            };
            if (v2 instanceof Right) {
                return new Right(v1(v2.value0));
            };
            throw new Error("Failed pattern match at Data.Either line 53, column 3 - line 53, column 34: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
});
var bifoldableEither = new Data_Bifoldable.Bifoldable(function (dictMonoid) {
    return function (v) {
        return function (v1) {
            return function (v2) {
                if (v2 instanceof Left) {
                    return v(v2.value0);
                };
                if (v2 instanceof Right) {
                    return v1(v2.value0);
                };
                throw new Error("Failed pattern match at Data.Either line 210, column 3 - line 210, column 31: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
            };
        };
    };
}, function (v) {
    return function (v1) {
        return function (z) {
            return function (v2) {
                if (v2 instanceof Left) {
                    return v(z)(v2.value0);
                };
                if (v2 instanceof Right) {
                    return v1(z)(v2.value0);
                };
                throw new Error("Failed pattern match at Data.Either line 208, column 3 - line 208, column 33: " + [ v.constructor.name, v1.constructor.name, z.constructor.name, v2.constructor.name ]);
            };
        };
    };
}, function (v) {
    return function (v1) {
        return function (z) {
            return function (v2) {
                if (v2 instanceof Left) {
                    return v(v2.value0)(z);
                };
                if (v2 instanceof Right) {
                    return v1(v2.value0)(z);
                };
                throw new Error("Failed pattern match at Data.Either line 206, column 3 - line 206, column 33: " + [ v.constructor.name, v1.constructor.name, z.constructor.name, v2.constructor.name ]);
            };
        };
    };
});
var bitraversableEither = new Data_Bitraversable.Bitraversable(function () {
    return bifoldableEither;
}, function () {
    return bifunctorEither;
}, function (dictApplicative) {
    return function (v) {
        if (v instanceof Left) {
            return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Left.create)(v.value0);
        };
        if (v instanceof Right) {
            return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Right.create)(v.value0);
        };
        throw new Error("Failed pattern match at Data.Either line 222, column 3 - line 222, column 35: " + [ v.constructor.name ]);
    };
}, function (dictApplicative) {
    return function (v) {
        return function (v1) {
            return function (v2) {
                if (v2 instanceof Left) {
                    return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Left.create)(v(v2.value0));
                };
                if (v2 instanceof Right) {
                    return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Right.create)(v1(v2.value0));
                };
                throw new Error("Failed pattern match at Data.Either line 220, column 3 - line 220, column 41: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
            };
        };
    };
});
var applyEither = new Control_Apply.Apply(function () {
    return functorEither;
}, function (v) {
    return function (v1) {
        if (v instanceof Left) {
            return new Left(v.value0);
        };
        if (v instanceof Right) {
            return Data_Functor.map(functorEither)(v.value0)(v1);
        };
        throw new Error("Failed pattern match at Data.Either line 89, column 3 - line 89, column 28: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var bindEither = new Control_Bind.Bind(function () {
    return applyEither;
}, either(function (e) {
    return function (v) {
        return new Left(e);
    };
})(function (a) {
    return function (f) {
        return f(a);
    };
}));
var semigroupEither = function (dictSemigroup) {
    return new Data_Semigroup.Semigroup(function (x) {
        return function (y) {
            return Control_Apply.apply(applyEither)(Data_Functor.map(functorEither)(Data_Semigroup.append(dictSemigroup))(x))(y);
        };
    });
};
var semiringEither = function (dictSemiring) {
    return new Data_Semiring.Semiring(function (x) {
        return function (y) {
            return Control_Apply.apply(applyEither)(Data_Functor.map(functorEither)(Data_Semiring.add(dictSemiring))(x))(y);
        };
    }, function (x) {
        return function (y) {
            return Control_Apply.apply(applyEither)(Data_Functor.map(functorEither)(Data_Semiring.mul(dictSemiring))(x))(y);
        };
    }, new Right(Data_Semiring.one(dictSemiring)), new Right(Data_Semiring.zero(dictSemiring)));
};
var applicativeEither = new Control_Applicative.Applicative(function () {
    return applyEither;
}, Right.create);
var monadEither = new Control_Monad.Monad(function () {
    return applicativeEither;
}, function () {
    return bindEither;
});
var altEither = new Control_Alt.Alt(function () {
    return functorEither;
}, function (v) {
    return function (v1) {
        if (v instanceof Left) {
            return v1;
        };
        return v;
    };
});
module.exports = {
    Left: Left, 
    Right: Right, 
    either: either, 
    fromLeft: fromLeft, 
    fromRight: fromRight, 
    isLeft: isLeft, 
    isRight: isRight, 
    functorEither: functorEither, 
    invariantEither: invariantEither, 
    bifunctorEither: bifunctorEither, 
    applyEither: applyEither, 
    applicativeEither: applicativeEither, 
    altEither: altEither, 
    bindEither: bindEither, 
    monadEither: monadEither, 
    extendEither: extendEither, 
    showEither: showEither, 
    eqEither: eqEither, 
    ordEither: ordEither, 
    boundedEither: boundedEither, 
    foldableEither: foldableEither, 
    bifoldableEither: bifoldableEither, 
    traversableEither: traversableEither, 
    bitraversableEither: bitraversableEither, 
    semiringEither: semiringEither, 
    semigroupEither: semigroupEither
};

},{"../Control.Alt":5,"../Control.Applicative":7,"../Control.Apply":9,"../Control.Bind":13,"../Control.Extend":16,"../Control.Monad":48,"../Data.Bifoldable":68,"../Data.Bifunctor":69,"../Data.Bitraversable":70,"../Data.Bounded":74,"../Data.Eq":79,"../Data.Foldable":84,"../Data.Function":97,"../Data.Functor":100,"../Data.Functor.Invariant":98,"../Data.Monoid":120,"../Data.Ord":127,"../Data.Ordering":128,"../Data.Semigroup":132,"../Data.Semiring":134,"../Data.Show":136,"../Data.Traversable":146}],78:[function(require,module,exports){
"use strict";

// module Data.Eq

exports.refEq = function (r1) {
  return function (r2) {
    return r1 === r2;
  };
};

exports.refIneq = function (r1) {
  return function (r2) {
    return r1 !== r2;
  };
};

exports.eqArrayImpl = function (f) {
  return function (xs) {
    return function (ys) {
      if (xs.length !== ys.length) return false;
      for (var i = 0; i < xs.length; i++) {
        if (!f(xs[i])(ys[i])) return false;
      }
      return true;
    };
  };
};

},{}],79:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Data_Unit = require("../Data.Unit");
var Data_Void = require("../Data.Void");
var Eq = function (eq) {
    this.eq = eq;
};
var eqVoid = new Eq(function (v) {
    return function (v1) {
        return true;
    };
});
var eqUnit = new Eq(function (v) {
    return function (v1) {
        return true;
    };
});
var eqString = new Eq($foreign.refEq);
var eqNumber = new Eq($foreign.refEq);
var eqInt = new Eq($foreign.refEq);
var eqChar = new Eq($foreign.refEq);
var eqBoolean = new Eq($foreign.refEq);
var eq = function (dict) {
    return dict.eq;
};
var eqArray = function (dictEq) {
    return new Eq($foreign.eqArrayImpl(eq(dictEq)));
};
var notEq = function (dictEq) {
    return function (x) {
        return function (y) {
            return eq(eqBoolean)(eq(dictEq)(x)(y))(false);
        };
    };
};
module.exports = {
    Eq: Eq, 
    eq: eq, 
    notEq: notEq, 
    eqBoolean: eqBoolean, 
    eqInt: eqInt, 
    eqNumber: eqNumber, 
    eqChar: eqChar, 
    eqString: eqString, 
    eqUnit: eqUnit, 
    eqVoid: eqVoid, 
    eqArray: eqArray
};

},{"../Data.Unit":151,"../Data.Void":152,"./foreign":78}],80:[function(require,module,exports){
"use strict";

// module Data.EuclideanRing

exports.intDegree = function (x) {
  return Math.abs(x);
};

exports.intDiv = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x / y | 0;
  };
};

exports.intMod = function (x) {
  return function (y) {
    return x % y;
  };
};

exports.numDiv = function (n1) {
  return function (n2) {
    return n1 / n2;
  };
};

},{}],81:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Data_CommutativeRing = require("../Data.CommutativeRing");
var Data_Ring = require("../Data.Ring");
var Data_Semiring = require("../Data.Semiring");
var Data_Unit = require("../Data.Unit");
var EuclideanRing = function (__superclass_Data$dotCommutativeRing$dotCommutativeRing_0, degree, div, mod) {
    this["__superclass_Data.CommutativeRing.CommutativeRing_0"] = __superclass_Data$dotCommutativeRing$dotCommutativeRing_0;
    this.degree = degree;
    this.div = div;
    this.mod = mod;
};
var mod = function (dict) {
    return dict.mod;
};
var euclideanRingUnit = new EuclideanRing(function () {
    return Data_CommutativeRing.commutativeRingUnit;
}, function (v) {
    return 1;
}, function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
}, function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
});
var euclideanRingNumber = new EuclideanRing(function () {
    return Data_CommutativeRing.commutativeRingNumber;
}, function (v) {
    return 1;
}, $foreign.numDiv, function (v) {
    return function (v1) {
        return 0.0;
    };
});
var euclideanRingInt = new EuclideanRing(function () {
    return Data_CommutativeRing.commutativeRingInt;
}, $foreign.intDegree, $foreign.intDiv, $foreign.intMod);
var div = function (dict) {
    return dict.div;
};
var degree = function (dict) {
    return dict.degree;
};
module.exports = {
    EuclideanRing: EuclideanRing, 
    degree: degree, 
    div: div, 
    mod: mod, 
    euclideanRingInt: euclideanRingInt, 
    euclideanRingNumber: euclideanRingNumber, 
    euclideanRingUnit: euclideanRingUnit
};

},{"../Data.CommutativeRing":75,"../Data.Ring":130,"../Data.Semiring":134,"../Data.Unit":151,"./foreign":80}],82:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Data_CommutativeRing = require("../Data.CommutativeRing");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Ring = require("../Data.Ring");
var Data_Semiring = require("../Data.Semiring");
var Data_Unit = require("../Data.Unit");
var Field = function (__superclass_Data$dotEuclideanRing$dotEuclideanRing_0) {
    this["__superclass_Data.EuclideanRing.EuclideanRing_0"] = __superclass_Data$dotEuclideanRing$dotEuclideanRing_0;
};
var fieldUnit = new Field(function () {
    return Data_EuclideanRing.euclideanRingUnit;
});
var fieldNumber = new Field(function () {
    return Data_EuclideanRing.euclideanRingNumber;
});
module.exports = {
    Field: Field, 
    fieldNumber: fieldNumber, 
    fieldUnit: fieldUnit
};

},{"../Data.CommutativeRing":75,"../Data.EuclideanRing":81,"../Data.Ring":130,"../Data.Semiring":134,"../Data.Unit":151}],83:[function(require,module,exports){
"use strict";

exports.foldrArray = function (f) {
  return function (init) {
    return function (xs) {
      var acc = init;
      var len = xs.length;
      for (var i = len - 1; i >= 0; i--) {
        acc = f(xs[i])(acc);
      }
      return acc;
    };
  };
};

exports.foldlArray = function (f) {
  return function (init) {
    return function (xs) {
      var acc = init;
      var len = xs.length;
      for (var i = 0; i < len; i++) {
        acc = f(acc)(xs[i]);
      }
      return acc;
    };
  };
};

},{}],84:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Plus = require("../Control.Plus");
var Data_BooleanAlgebra = require("../Data.BooleanAlgebra");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Maybe = require("../Data.Maybe");
var Data_Maybe_First = require("../Data.Maybe.First");
var Data_Maybe_Last = require("../Data.Maybe.Last");
var Data_Monoid = require("../Data.Monoid");
var Data_Monoid_Additive = require("../Data.Monoid.Additive");
var Data_Monoid_Conj = require("../Data.Monoid.Conj");
var Data_Monoid_Disj = require("../Data.Monoid.Disj");
var Data_Monoid_Dual = require("../Data.Monoid.Dual");
var Data_Monoid_Endo = require("../Data.Monoid.Endo");
var Data_Monoid_Multiplicative = require("../Data.Monoid.Multiplicative");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Semiring = require("../Data.Semiring");
var Data_Unit = require("../Data.Unit");
var Control_Alt = require("../Control.Alt");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Control_Category = require("../Control.Category");
var Data_Semigroup = require("../Data.Semigroup");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Foldable = function (foldMap, foldl, foldr) {
    this.foldMap = foldMap;
    this.foldl = foldl;
    this.foldr = foldr;
};
var foldr = function (dict) {
    return dict.foldr;
};
var oneOf = function (dictFoldable) {
    return function (dictPlus) {
        return foldr(dictFoldable)(Control_Alt.alt(dictPlus["__superclass_Control.Alt.Alt_0"]()))(Control_Plus.empty(dictPlus));
    };
};
var traverse_ = function (dictApplicative) {
    return function (dictFoldable) {
        return function (f) {
            return foldr(dictFoldable)(function ($164) {
                return Control_Apply.applySecond(dictApplicative["__superclass_Control.Apply.Apply_0"]())(f($164));
            })(Control_Applicative.pure(dictApplicative)(Data_Unit.unit));
        };
    };
};
var for_ = function (dictApplicative) {
    return function (dictFoldable) {
        return Data_Function.flip(traverse_(dictApplicative)(dictFoldable));
    };
};
var sequence_ = function (dictApplicative) {
    return function (dictFoldable) {
        return traverse_(dictApplicative)(dictFoldable)(Control_Category.id(Control_Category.categoryFn));
    };
};
var foldl = function (dict) {
    return dict.foldl;
};
var intercalate = function (dictFoldable) {
    return function (dictMonoid) {
        return function (sep) {
            return function (xs) {
                var go = function (v) {
                    return function (x) {
                        if (v.init) {
                            return {
                                init: false, 
                                acc: x
                            };
                        };
                        return {
                            init: false, 
                            acc: Data_Semigroup.append(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]())(v.acc)(Data_Semigroup.append(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]())(sep)(x))
                        };
                    };
                };
                return (foldl(dictFoldable)(go)({
                    init: true, 
                    acc: Data_Monoid.mempty(dictMonoid)
                })(xs)).acc;
            };
        };
    };
};
var maximumBy = function (dictFoldable) {
    return function (cmp) {
        var max$prime = function (v) {
            return function (v1) {
                if (v instanceof Data_Maybe.Nothing) {
                    return new Data_Maybe.Just(v1);
                };
                if (v instanceof Data_Maybe.Just) {
                    return new Data_Maybe.Just((function () {
                        var $89 = Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(v.value0)(v1))(Data_Ordering.GT.value);
                        if ($89) {
                            return v.value0;
                        };
                        if (!$89) {
                            return v1;
                        };
                        throw new Error("Failed pattern match at Data.Foldable line 290, column 27 - line 290, column 57: " + [ $89.constructor.name ]);
                    })());
                };
                throw new Error("Failed pattern match at Data.Foldable line 289, column 3 - line 289, column 27: " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
        return foldl(dictFoldable)(max$prime)(Data_Maybe.Nothing.value);
    };
};
var maximum = function (dictOrd) {
    return function (dictFoldable) {
        return maximumBy(dictFoldable)(Data_Ord.compare(dictOrd));
    };
};
var minimumBy = function (dictFoldable) {
    return function (cmp) {
        var min$prime = function (v) {
            return function (v1) {
                if (v instanceof Data_Maybe.Nothing) {
                    return new Data_Maybe.Just(v1);
                };
                if (v instanceof Data_Maybe.Just) {
                    return new Data_Maybe.Just((function () {
                        var $93 = Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(v.value0)(v1))(Data_Ordering.LT.value);
                        if ($93) {
                            return v.value0;
                        };
                        if (!$93) {
                            return v1;
                        };
                        throw new Error("Failed pattern match at Data.Foldable line 303, column 27 - line 303, column 57: " + [ $93.constructor.name ]);
                    })());
                };
                throw new Error("Failed pattern match at Data.Foldable line 302, column 3 - line 302, column 27: " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
        return foldl(dictFoldable)(min$prime)(Data_Maybe.Nothing.value);
    };
};
var minimum = function (dictOrd) {
    return function (dictFoldable) {
        return minimumBy(dictFoldable)(Data_Ord.compare(dictOrd));
    };
};
var product = function (dictFoldable) {
    return function (dictSemiring) {
        return foldl(dictFoldable)(Data_Semiring.mul(dictSemiring))(Data_Semiring.one(dictSemiring));
    };
};
var sum = function (dictFoldable) {
    return function (dictSemiring) {
        return foldl(dictFoldable)(Data_Semiring.add(dictSemiring))(Data_Semiring.zero(dictSemiring));
    };
};
var foldableMultiplicative = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldableMaybe = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            if (v instanceof Data_Maybe.Nothing) {
                return Data_Monoid.mempty(dictMonoid);
            };
            if (v instanceof Data_Maybe.Just) {
                return f(v.value0);
            };
            throw new Error("Failed pattern match at Data.Foldable line 132, column 3 - line 132, column 30: " + [ f.constructor.name, v.constructor.name ]);
        };
    };
}, function (v) {
    return function (z) {
        return function (v1) {
            if (v1 instanceof Data_Maybe.Nothing) {
                return z;
            };
            if (v1 instanceof Data_Maybe.Just) {
                return v(z)(v1.value0);
            };
            throw new Error("Failed pattern match at Data.Foldable line 130, column 3 - line 130, column 25: " + [ v.constructor.name, z.constructor.name, v1.constructor.name ]);
        };
    };
}, function (v) {
    return function (z) {
        return function (v1) {
            if (v1 instanceof Data_Maybe.Nothing) {
                return z;
            };
            if (v1 instanceof Data_Maybe.Just) {
                return v(v1.value0)(z);
            };
            throw new Error("Failed pattern match at Data.Foldable line 128, column 3 - line 128, column 25: " + [ v.constructor.name, z.constructor.name, v1.constructor.name ]);
        };
    };
});
var foldableDual = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldableDisj = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldableConj = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldableAdditive = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldMapDefaultR = function (dictFoldable) {
    return function (dictMonoid) {
        return function (f) {
            return function (xs) {
                return foldr(dictFoldable)(function (x) {
                    return function (acc) {
                        return Data_Semigroup.append(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]())(f(x))(acc);
                    };
                })(Data_Monoid.mempty(dictMonoid))(xs);
            };
        };
    };
};
var foldableArray = new Foldable(function (dictMonoid) {
    return foldMapDefaultR(foldableArray)(dictMonoid);
}, $foreign.foldlArray, $foreign.foldrArray);
var foldMapDefaultL = function (dictFoldable) {
    return function (dictMonoid) {
        return function (f) {
            return function (xs) {
                return foldl(dictFoldable)(function (acc) {
                    return function (x) {
                        return Data_Semigroup.append(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]())(f(x))(acc);
                    };
                })(Data_Monoid.mempty(dictMonoid))(xs);
            };
        };
    };
};
var foldMap = function (dict) {
    return dict.foldMap;
};
var foldableFirst = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return foldMap(foldableMaybe)(dictMonoid)(f)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return foldl(foldableMaybe)(f)(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return foldr(foldableMaybe)(f)(z)(v);
        };
    };
});
var foldableLast = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return foldMap(foldableMaybe)(dictMonoid)(f)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return foldl(foldableMaybe)(f)(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return foldr(foldableMaybe)(f)(z)(v);
        };
    };
});
var foldlDefault = function (dictFoldable) {
    return function (c) {
        return function (u) {
            return function (xs) {
                return Data_Monoid_Endo.runEndo(Data_Monoid_Dual.runDual(foldMap(dictFoldable)(Data_Monoid_Dual.monoidDual(Data_Monoid_Endo.monoidEndo))(function ($165) {
                    return Data_Monoid_Dual.Dual(Data_Monoid_Endo.Endo(Data_Function.flip(c)($165)));
                })(xs)))(u);
            };
        };
    };
};
var foldrDefault = function (dictFoldable) {
    return function (c) {
        return function (u) {
            return function (xs) {
                return Data_Monoid_Endo.runEndo(foldMap(dictFoldable)(Data_Monoid_Endo.monoidEndo)(function ($166) {
                    return Data_Monoid_Endo.Endo(c($166));
                })(xs))(u);
            };
        };
    };
};
var fold = function (dictFoldable) {
    return function (dictMonoid) {
        return foldMap(dictFoldable)(dictMonoid)(Control_Category.id(Control_Category.categoryFn));
    };
};
var find = function (dictFoldable) {
    return function (p) {
        var go = function (v) {
            return function (v1) {
                if (v instanceof Data_Maybe.Nothing && p(v1)) {
                    return new Data_Maybe.Just(v1);
                };
                return v;
            };
        };
        return foldl(dictFoldable)(go)(Data_Maybe.Nothing.value);
    };
};
var any = function (dictFoldable) {
    return function (dictBooleanAlgebra) {
        return function (p) {
            return function ($167) {
                return Data_Monoid_Disj.runDisj(foldMap(dictFoldable)(Data_Monoid_Disj.monoidDisj(dictBooleanAlgebra["__superclass_Data.HeytingAlgebra.HeytingAlgebra_0"]()))(function ($168) {
                    return Data_Monoid_Disj.Disj(p($168));
                })($167));
            };
        };
    };
};
var elem = function (dictFoldable) {
    return function (dictEq) {
        return function ($169) {
            return any(dictFoldable)(Data_BooleanAlgebra.booleanAlgebraBoolean)(Data_Eq.eq(dictEq)($169));
        };
    };
};
var notElem = function (dictFoldable) {
    return function (dictEq) {
        return function (x) {
            return function ($170) {
                return !elem(dictFoldable)(dictEq)(x)($170);
            };
        };
    };
};
var or = function (dictFoldable) {
    return function (dictBooleanAlgebra) {
        return any(dictFoldable)(dictBooleanAlgebra)(Control_Category.id(Control_Category.categoryFn));
    };
};
var all = function (dictFoldable) {
    return function (dictBooleanAlgebra) {
        return function (p) {
            return function ($171) {
                return Data_Monoid_Conj.runConj(foldMap(dictFoldable)(Data_Monoid_Conj.monoidConj(dictBooleanAlgebra["__superclass_Data.HeytingAlgebra.HeytingAlgebra_0"]()))(function ($172) {
                    return Data_Monoid_Conj.Conj(p($172));
                })($171));
            };
        };
    };
};
var and = function (dictFoldable) {
    return function (dictBooleanAlgebra) {
        return all(dictFoldable)(dictBooleanAlgebra)(Control_Category.id(Control_Category.categoryFn));
    };
};
module.exports = {
    Foldable: Foldable, 
    all: all, 
    and: and, 
    any: any, 
    elem: elem, 
    find: find, 
    fold: fold, 
    foldMap: foldMap, 
    foldMapDefaultL: foldMapDefaultL, 
    foldMapDefaultR: foldMapDefaultR, 
    foldl: foldl, 
    foldlDefault: foldlDefault, 
    foldr: foldr, 
    foldrDefault: foldrDefault, 
    for_: for_, 
    intercalate: intercalate, 
    maximum: maximum, 
    maximumBy: maximumBy, 
    minimum: minimum, 
    minimumBy: minimumBy, 
    notElem: notElem, 
    oneOf: oneOf, 
    or: or, 
    product: product, 
    sequence_: sequence_, 
    sum: sum, 
    traverse_: traverse_, 
    foldableArray: foldableArray, 
    foldableMaybe: foldableMaybe, 
    foldableFirst: foldableFirst, 
    foldableLast: foldableLast, 
    foldableAdditive: foldableAdditive, 
    foldableDual: foldableDual, 
    foldableDisj: foldableDisj, 
    foldableConj: foldableConj, 
    foldableMultiplicative: foldableMultiplicative
};

},{"../Control.Alt":5,"../Control.Applicative":7,"../Control.Apply":9,"../Control.Category":14,"../Control.Plus":52,"../Control.Semigroupoid":53,"../Data.BooleanAlgebra":72,"../Data.Eq":79,"../Data.Function":97,"../Data.HeytingAlgebra":104,"../Data.Maybe":113,"../Data.Maybe.First":111,"../Data.Maybe.Last":112,"../Data.Monoid":120,"../Data.Monoid.Additive":114,"../Data.Monoid.Conj":115,"../Data.Monoid.Disj":116,"../Data.Monoid.Dual":117,"../Data.Monoid.Endo":118,"../Data.Monoid.Multiplicative":119,"../Data.Ord":127,"../Data.Ordering":128,"../Data.Semigroup":132,"../Data.Semiring":134,"../Data.Unit":151,"./foreign":83}],85:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Data_Array = require("../Data.Array");
var Data_Either = require("../Data.Either");
var Data_Foreign = require("../Data.Foreign");
var Data_Foreign_Index = require("../Data.Foreign.Index");
var Data_Foreign_Null = require("../Data.Foreign.Null");
var Data_Foreign_NullOrUndefined = require("../Data.Foreign.NullOrUndefined");
var Data_Foreign_Undefined = require("../Data.Foreign.Undefined");
var Data_Traversable = require("../Data.Traversable");
var Data_Maybe = require("../Data.Maybe");
var Control_Applicative = require("../Control.Applicative");
var Data_Semiring = require("../Data.Semiring");
var Control_Bind = require("../Control.Bind");
var Control_Category = require("../Control.Category");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Functor = require("../Data.Functor");
var IsForeign = function (read) {
    this.read = read;
};
var AsForeign = function (write) {
    this.write = write;
};
var write = function (dict) {
    return dict.write;
};
var writeProp = function (dictAsForeign) {
    return function (k) {
        return function (v) {
            return {
                key: k, 
                value: write(dictAsForeign)(v)
            };
        };
    };
};
var undefinedAsForeign = function (dictAsForeign) {
    return new AsForeign(function (v) {
        return Data_Maybe.maybe(Data_Foreign_Undefined.writeUndefined)(write(dictAsForeign))(v);
    });
};
var stringIsForeign = new IsForeign(Data_Foreign.readString);
var stringAsForeign = new AsForeign(Data_Foreign.toForeign);
var read = function (dict) {
    return dict.read;
};
var readJSON = function (dictIsForeign) {
    return function (json) {
        return Control_Bind.bind(Data_Either.bindEither)(Data_Foreign.parseJSON(json))(read(dictIsForeign));
    };
};
var readWith = function (dictIsForeign) {
    return function (f) {
        return function (value) {
            return Data_Either.either(function ($19) {
                return Data_Either.Left.create(f($19));
            })(Data_Either.Right.create)(read(dictIsForeign)(value));
        };
    };
};
var readProp = function (dictIsForeign) {
    return function (dictIndex) {
        return function (prop) {
            return function (value) {
                return Control_Bind.bind(Data_Either.bindEither)(Data_Foreign_Index.ix(dictIndex)(value)(prop))(readWith(dictIsForeign)(Data_Foreign_Index.errorAt(dictIndex)(prop)));
            };
        };
    };
};
var undefinedIsForeign = function (dictIsForeign) {
    return new IsForeign(Data_Foreign_Undefined.readUndefined(read(dictIsForeign)));
};
var numberIsForeign = new IsForeign(Data_Foreign.readNumber);
var numberAsForeign = new AsForeign(Data_Foreign.toForeign);
var nullOrUndefinedIsForeign = function (dictIsForeign) {
    return new IsForeign(Data_Foreign_NullOrUndefined.readNullOrUndefined(read(dictIsForeign)));
};
var nullIsForeign = function (dictIsForeign) {
    return new IsForeign(Data_Foreign_Null.readNull(read(dictIsForeign)));
};
var nullAsForeign = function (dictAsForeign) {
    return new AsForeign(function (v) {
        return Data_Maybe.maybe(Data_Foreign_Null.writeNull)(write(dictAsForeign))(v);
    });
};
var nullOrUndefinedAsForeign = function (dictAsForeign) {
    return new AsForeign(function (v) {
        return write(nullAsForeign(dictAsForeign))(v);
    });
};
var intIsForeign = new IsForeign(Data_Foreign.readInt);
var intAsForeign = new AsForeign(Data_Foreign.toForeign);
var foreignIsForeign = new IsForeign(Control_Applicative.pure(Data_Either.applicativeEither));
var foreignAsForeign = new AsForeign(Control_Category.id(Control_Category.categoryFn));
var charIsForeign = new IsForeign(Data_Foreign.readChar);
var charAsForeign = new AsForeign(Data_Foreign.toForeign);
var booleanIsForeign = new IsForeign(Data_Foreign.readBoolean);
var booleanAsForeign = new AsForeign(Data_Foreign.toForeign);
var arrayIsForeign = function (dictIsForeign) {
    return new IsForeign(function (value) {
        var readElement = function (i) {
            return function (value1) {
                return readWith(dictIsForeign)(Data_Foreign.ErrorAtIndex.create(i))(value1);
            };
        };
        var readElements = function (arr) {
            return Data_Traversable.sequence(Data_Traversable.traversableArray)(Data_Either.applicativeEither)(Data_Array.zipWith(readElement)(Data_Array.range(0)(Data_Array.length(arr)))(arr));
        };
        return Control_Bind.bind(Data_Either.bindEither)(Data_Foreign.readArray(value))(readElements);
    });
};
var arrayAsForeign = function (dictAsForeign) {
    return new AsForeign(function ($20) {
        return Data_Foreign.toForeign(Data_Functor.map(Data_Functor.functorArray)(write(dictAsForeign))($20));
    });
};
module.exports = {
    AsForeign: AsForeign, 
    IsForeign: IsForeign, 
    read: read, 
    readJSON: readJSON, 
    readProp: readProp, 
    readWith: readWith, 
    write: write, 
    writeProp: writeProp, 
    foreignIsForeign: foreignIsForeign, 
    stringIsForeign: stringIsForeign, 
    charIsForeign: charIsForeign, 
    booleanIsForeign: booleanIsForeign, 
    numberIsForeign: numberIsForeign, 
    intIsForeign: intIsForeign, 
    arrayIsForeign: arrayIsForeign, 
    nullIsForeign: nullIsForeign, 
    undefinedIsForeign: undefinedIsForeign, 
    nullOrUndefinedIsForeign: nullOrUndefinedIsForeign, 
    foreignAsForeign: foreignAsForeign, 
    stringAsForeign: stringAsForeign, 
    charAsForeign: charAsForeign, 
    booleanAsForeign: booleanAsForeign, 
    numberAsForeign: numberAsForeign, 
    intAsForeign: intAsForeign, 
    arrayAsForeign: arrayAsForeign, 
    nullAsForeign: nullAsForeign, 
    undefinedAsForeign: undefinedAsForeign, 
    nullOrUndefinedAsForeign: nullOrUndefinedAsForeign
};

},{"../Control.Applicative":7,"../Control.Bind":13,"../Control.Category":14,"../Control.Semigroupoid":53,"../Data.Array":67,"../Data.Either":77,"../Data.Foreign":94,"../Data.Foreign.Index":87,"../Data.Foreign.Null":89,"../Data.Foreign.NullOrUndefined":90,"../Data.Foreign.Undefined":92,"../Data.Functor":100,"../Data.Maybe":113,"../Data.Semiring":134,"../Data.Traversable":146,"../Prelude":162}],86:[function(require,module,exports){
/* global exports */
"use strict";

// jshint maxparams: 4
exports.unsafeReadPropImpl = function (f, s, key, value) {
  return value == null ? f : s(value[key]);
};

// jshint maxparams: 2
exports.unsafeHasOwnProperty = function (prop, value) {
  return Object.prototype.hasOwnProperty.call(value, prop);
};

exports.unsafeHasProperty = function (prop, value) {
  return prop in value;
};

},{}],87:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Either = require("../Data.Either");
var Data_Foreign = require("../Data.Foreign");
var Data_Function_Uncurried = require("../Data.Function.Uncurried");
var Data_Function = require("../Data.Function");
var Control_Applicative = require("../Control.Applicative");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Eq = require("../Data.Eq");
var Index = function (errorAt, hasOwnProperty, hasProperty, ix) {
    this.errorAt = errorAt;
    this.hasOwnProperty = hasOwnProperty;
    this.hasProperty = hasProperty;
    this.ix = ix;
};
var unsafeReadProp = function (k) {
    return function (value) {
        return $foreign.unsafeReadPropImpl(new Data_Either.Left(new Data_Foreign.TypeMismatch("object", Data_Foreign.typeOf(value))), Control_Applicative.pure(Data_Either.applicativeEither), k, value);
    };
};
var prop = unsafeReadProp;
var ix = function (dict) {
    return dict.ix;
};
var index = unsafeReadProp;
var hasPropertyImpl = function (v) {
    return function (value) {
        if (Data_Foreign.isNull(value)) {
            return false;
        };
        if (Data_Foreign.isUndefined(value)) {
            return false;
        };
        if (Data_Foreign.typeOf(value) === "object" || Data_Foreign.typeOf(value) === "function") {
            return $foreign.unsafeHasProperty(v, value);
        };
        return false;
    };
};
var hasProperty = function (dict) {
    return dict.hasProperty;
};
var hasOwnPropertyImpl = function (v) {
    return function (value) {
        if (Data_Foreign.isNull(value)) {
            return false;
        };
        if (Data_Foreign.isUndefined(value)) {
            return false;
        };
        if (Data_Foreign.typeOf(value) === "object" || Data_Foreign.typeOf(value) === "function") {
            return $foreign.unsafeHasOwnProperty(v, value);
        };
        return false;
    };
};
var indexInt = new Index(Data_Foreign.ErrorAtIndex.create, hasOwnPropertyImpl, hasPropertyImpl, Data_Function.flip(index));
var indexString = new Index(Data_Foreign.ErrorAtProperty.create, hasOwnPropertyImpl, hasPropertyImpl, Data_Function.flip(prop));
var hasOwnProperty = function (dict) {
    return dict.hasOwnProperty;
};
var errorAt = function (dict) {
    return dict.errorAt;
};
module.exports = {
    Index: Index, 
    errorAt: errorAt, 
    hasOwnProperty: hasOwnProperty, 
    hasProperty: hasProperty, 
    index: index, 
    ix: ix, 
    prop: prop, 
    indexString: indexString, 
    indexInt: indexInt
};

},{"../Control.Applicative":7,"../Data.Either":77,"../Data.Eq":79,"../Data.Foreign":94,"../Data.Function":97,"../Data.Function.Uncurried":96,"../Data.HeytingAlgebra":104,"../Prelude":162,"./foreign":86}],88:[function(require,module,exports){
/* global exports */
"use strict";

exports.writeNull = null;

},{}],89:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Maybe = require("../Data.Maybe");
var Data_Foreign = require("../Data.Foreign");
var Control_Applicative = require("../Control.Applicative");
var Data_Either = require("../Data.Either");
var Data_Functor = require("../Data.Functor");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Null = function (x) {
    return x;
};
var unNull = function (v) {
    return v;
};
var readNull = function (v) {
    return function (value) {
        if (Data_Foreign.isNull(value)) {
            return Control_Applicative.pure(Data_Either.applicativeEither)(Data_Maybe.Nothing.value);
        };
        return Data_Functor.map(Data_Either.functorEither)(function ($5) {
            return Null(Data_Maybe.Just.create($5));
        })(v(value));
    };
};
module.exports = {
    Null: Null, 
    readNull: readNull, 
    unNull: unNull, 
    writeNull: $foreign.writeNull
};

},{"../Control.Applicative":7,"../Control.Semigroupoid":53,"../Data.Either":77,"../Data.Foreign":94,"../Data.Functor":100,"../Data.Maybe":113,"../Prelude":162,"./foreign":88}],90:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Data_Maybe = require("../Data.Maybe");
var Data_Foreign = require("../Data.Foreign");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Control_Applicative = require("../Control.Applicative");
var Data_Either = require("../Data.Either");
var Data_Functor = require("../Data.Functor");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var NullOrUndefined = function (x) {
    return x;
};
var unNullOrUndefined = function (v) {
    return v;
};
var readNullOrUndefined = function (v) {
    return function (value) {
        if (Data_Foreign.isNull(value) || Data_Foreign.isUndefined(value)) {
            return Control_Applicative.pure(Data_Either.applicativeEither)(Data_Maybe.Nothing.value);
        };
        return Data_Functor.map(Data_Either.functorEither)(function ($5) {
            return NullOrUndefined(Data_Maybe.Just.create($5));
        })(v(value));
    };
};
module.exports = {
    NullOrUndefined: NullOrUndefined, 
    readNullOrUndefined: readNullOrUndefined, 
    unNullOrUndefined: unNullOrUndefined
};

},{"../Control.Applicative":7,"../Control.Semigroupoid":53,"../Data.Either":77,"../Data.Foreign":94,"../Data.Functor":100,"../Data.HeytingAlgebra":104,"../Data.Maybe":113,"../Prelude":162}],91:[function(require,module,exports){
/* global exports */
"use strict";

exports.writeUndefined = undefined;

},{}],92:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Maybe = require("../Data.Maybe");
var Data_Foreign = require("../Data.Foreign");
var Control_Applicative = require("../Control.Applicative");
var Data_Either = require("../Data.Either");
var Data_Functor = require("../Data.Functor");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Undefined = function (x) {
    return x;
};
var unUndefined = function (v) {
    return v;
};
var readUndefined = function (v) {
    return function (value) {
        if (Data_Foreign.isUndefined(value)) {
            return Control_Applicative.pure(Data_Either.applicativeEither)(Data_Maybe.Nothing.value);
        };
        return Data_Functor.map(Data_Either.functorEither)(function ($5) {
            return Undefined(Data_Maybe.Just.create($5));
        })(v(value));
    };
};
module.exports = {
    Undefined: Undefined, 
    readUndefined: readUndefined, 
    unUndefined: unUndefined, 
    writeUndefined: $foreign.writeUndefined
};

},{"../Control.Applicative":7,"../Control.Semigroupoid":53,"../Data.Either":77,"../Data.Foreign":94,"../Data.Functor":100,"../Data.Maybe":113,"../Prelude":162,"./foreign":91}],93:[function(require,module,exports){
/* global exports */
"use strict";

// jshint maxparams: 3
exports.parseJSONImpl = function (left, right, str) {
  try {
    return right(JSON.parse(str));
  } catch (e) {
    return left(e.toString());
  }
};
// jshint maxparams: 1

exports.toForeign = function (value) {
  return value;
};

exports.unsafeFromForeign = function (value) {
  return value;
};

exports.typeOf = function (value) {
  return typeof value;
};

exports.tagOf = function (value) {
  return Object.prototype.toString.call(value).slice(8, -1);
};

exports.isNull = function (value) {
  return value === null;
};

exports.isUndefined = function (value) {
  return value === undefined;
};

exports.isArray = Array.isArray || function (value) {
  return Object.prototype.toString.call(value) === "[object Array]";
};

exports.writeObject = function (fields) {
  var record = {};
  for (var i = 0; i < fields.length; i++) {
    record[fields[i].key] = fields[i].value;
  }
  return record;
};

},{}],94:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Either = require("../Data.Either");
var Data_Function_Uncurried = require("../Data.Function.Uncurried");
var Data_Int = require("../Data.Int");
var Data_Maybe = require("../Data.Maybe");
var Data_String = require("../Data.String");
var Data_Show = require("../Data.Show");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Eq = require("../Data.Eq");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Control_Applicative = require("../Control.Applicative");
var Data_Function = require("../Data.Function");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Prop = function (x) {
    return x;
};
var TypeMismatch = (function () {
    function TypeMismatch(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    TypeMismatch.create = function (value0) {
        return function (value1) {
            return new TypeMismatch(value0, value1);
        };
    };
    return TypeMismatch;
})();
var ErrorAtIndex = (function () {
    function ErrorAtIndex(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ErrorAtIndex.create = function (value0) {
        return function (value1) {
            return new ErrorAtIndex(value0, value1);
        };
    };
    return ErrorAtIndex;
})();
var ErrorAtProperty = (function () {
    function ErrorAtProperty(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ErrorAtProperty.create = function (value0) {
        return function (value1) {
            return new ErrorAtProperty(value0, value1);
        };
    };
    return ErrorAtProperty;
})();
var JSONError = (function () {
    function JSONError(value0) {
        this.value0 = value0;
    };
    JSONError.create = function (value0) {
        return new JSONError(value0);
    };
    return JSONError;
})();
var unsafeReadTagged = function (tag) {
    return function (value) {
        if ($foreign.tagOf(value) === tag) {
            return Control_Applicative.pure(Data_Either.applicativeEither)($foreign.unsafeFromForeign(value));
        };
        return new Data_Either.Left(new TypeMismatch(tag, $foreign.tagOf(value)));
    };
};
var showForeignError = new Data_Show.Show(function (v) {
    if (v instanceof TypeMismatch) {
        return "Type mismatch: expected " + (v.value0 + (", found " + v.value1));
    };
    if (v instanceof ErrorAtIndex) {
        return "Error at array index " + (Data_Show.show(Data_Show.showInt)(v.value0) + (": " + Data_Show.show(showForeignError)(v.value1)));
    };
    if (v instanceof ErrorAtProperty) {
        return "Error at property " + (Data_Show.show(Data_Show.showString)(v.value0) + (": " + Data_Show.show(showForeignError)(v.value1)));
    };
    if (v instanceof JSONError) {
        return "JSON error: " + v.value0;
    };
    throw new Error("Failed pattern match at Data.Foreign line 55, column 3 - line 55, column 87: " + [ v.constructor.name ]);
});
var readString = unsafeReadTagged("String");
var readNumber = unsafeReadTagged("Number");
var readInt = function (value) {
    var error = Data_Function.apply(Data_Either.Left.create)(new TypeMismatch("Int", $foreign.tagOf(value)));
    var fromNumber = function ($91) {
        return Data_Maybe.maybe(error)(Control_Applicative.pure(Data_Either.applicativeEither))(Data_Int.fromNumber($91));
    };
    return Data_Either.either(Data_Function["const"](error))(fromNumber)(readNumber(value));
};
var readChar = function (value) {
    var error = Data_Function.apply(Data_Either.Left.create)(new TypeMismatch("Char", $foreign.tagOf(value)));
    var fromString = function ($92) {
        return Data_Maybe.maybe(error)(Control_Applicative.pure(Data_Either.applicativeEither))(Data_String.toChar($92));
    };
    return Data_Either.either(Data_Function["const"](error))(fromString)(readString(value));
};
var readBoolean = unsafeReadTagged("Boolean");
var readArray = function (value) {
    if ($foreign.isArray(value)) {
        return Data_Function.apply(Control_Applicative.pure(Data_Either.applicativeEither))($foreign.unsafeFromForeign(value));
    };
    return new Data_Either.Left(new TypeMismatch("array", $foreign.tagOf(value)));
};
var parseJSON = function (json) {
    return $foreign.parseJSONImpl(function ($93) {
        return Data_Either.Left.create(JSONError.create($93));
    }, Data_Either.Right.create, json);
};
var eqForeignError = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof TypeMismatch && y instanceof TypeMismatch) {
            return x.value0 === y.value0 && x.value1 === y.value1;
        };
        if (x instanceof ErrorAtIndex && y instanceof ErrorAtIndex) {
            return x.value0 === y.value0 && Data_Eq.eq(eqForeignError)(x.value1)(y.value1);
        };
        if (x instanceof ErrorAtProperty && y instanceof ErrorAtProperty) {
            return x.value0 === y.value0 && Data_Eq.eq(eqForeignError)(x.value1)(y.value1);
        };
        if (x instanceof JSONError && y instanceof JSONError) {
            return x.value0 === y.value0;
        };
        return false;
    };
});
var ordForeignError = new Data_Ord.Ord(function () {
    return eqForeignError;
}, function (x) {
    return function (y) {
        if (x instanceof TypeMismatch && y instanceof TypeMismatch) {
            var $62 = Data_Ord.compare(Data_Ord.ordString)(x.value0)(y.value0);
            if ($62 instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if ($62 instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            return Data_Ord.compare(Data_Ord.ordString)(x.value1)(y.value1);
        };
        if (x instanceof TypeMismatch) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof TypeMismatch) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof ErrorAtIndex && y instanceof ErrorAtIndex) {
            var $71 = Data_Ord.compare(Data_Ord.ordInt)(x.value0)(y.value0);
            if ($71 instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if ($71 instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            return Data_Ord.compare(ordForeignError)(x.value1)(y.value1);
        };
        if (x instanceof ErrorAtIndex) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof ErrorAtIndex) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof ErrorAtProperty && y instanceof ErrorAtProperty) {
            var $80 = Data_Ord.compare(Data_Ord.ordString)(x.value0)(y.value0);
            if ($80 instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if ($80 instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            return Data_Ord.compare(ordForeignError)(x.value1)(y.value1);
        };
        if (x instanceof ErrorAtProperty) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof ErrorAtProperty) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof JSONError && y instanceof JSONError) {
            return Data_Ord.compare(Data_Ord.ordString)(x.value0)(y.value0);
        };
        throw new Error("Failed pattern match: " + [ x.constructor.name, y.constructor.name ]);
    };
});
module.exports = {
    TypeMismatch: TypeMismatch, 
    ErrorAtIndex: ErrorAtIndex, 
    ErrorAtProperty: ErrorAtProperty, 
    JSONError: JSONError, 
    Prop: Prop, 
    parseJSON: parseJSON, 
    readArray: readArray, 
    readBoolean: readBoolean, 
    readChar: readChar, 
    readInt: readInt, 
    readNumber: readNumber, 
    readString: readString, 
    unsafeReadTagged: unsafeReadTagged, 
    showForeignError: showForeignError, 
    eqForeignError: eqForeignError, 
    ordForeignError: ordForeignError, 
    isArray: $foreign.isArray, 
    isNull: $foreign.isNull, 
    isUndefined: $foreign.isUndefined, 
    tagOf: $foreign.tagOf, 
    toForeign: $foreign.toForeign, 
    typeOf: $foreign.typeOf, 
    unsafeFromForeign: $foreign.unsafeFromForeign, 
    writeObject: $foreign.writeObject
};

},{"../Control.Applicative":7,"../Control.Semigroupoid":53,"../Data.Either":77,"../Data.Eq":79,"../Data.Function":97,"../Data.Function.Uncurried":96,"../Data.HeytingAlgebra":104,"../Data.Int":109,"../Data.Maybe":113,"../Data.Ord":127,"../Data.Ordering":128,"../Data.Semigroup":132,"../Data.Show":136,"../Data.String":144,"../Prelude":162,"./foreign":93}],95:[function(require,module,exports){
"use strict";

// module Data.Function.Uncurried

exports.mkFn0 = function (fn) {
  return function () {
    return fn({});
  };
};

exports.mkFn1 = function (fn) {
  return function (a) {
    return fn(a);
  };
};

exports.mkFn2 = function (fn) {
  /* jshint maxparams: 2 */
  return function (a, b) {
    return fn(a)(b);
  };
};

exports.mkFn3 = function (fn) {
  /* jshint maxparams: 3 */
  return function (a, b, c) {
    return fn(a)(b)(c);
  };
};

exports.mkFn4 = function (fn) {
  /* jshint maxparams: 4 */
  return function (a, b, c, d) {
    return fn(a)(b)(c)(d);
  };
};

exports.mkFn5 = function (fn) {
  /* jshint maxparams: 5 */
  return function (a, b, c, d, e) {
    return fn(a)(b)(c)(d)(e);
  };
};

exports.mkFn6 = function (fn) {
  /* jshint maxparams: 6 */
  return function (a, b, c, d, e, f) {
    return fn(a)(b)(c)(d)(e)(f);
  };
};

exports.mkFn7 = function (fn) {
  /* jshint maxparams: 7 */
  return function (a, b, c, d, e, f, g) {
    return fn(a)(b)(c)(d)(e)(f)(g);
  };
};

exports.mkFn8 = function (fn) {
  /* jshint maxparams: 8 */
  return function (a, b, c, d, e, f, g, h) {
    return fn(a)(b)(c)(d)(e)(f)(g)(h);
  };
};

exports.mkFn9 = function (fn) {
  /* jshint maxparams: 9 */
  return function (a, b, c, d, e, f, g, h, i) {
    return fn(a)(b)(c)(d)(e)(f)(g)(h)(i);
  };
};

exports.mkFn10 = function (fn) {
  /* jshint maxparams: 10 */
  return function (a, b, c, d, e, f, g, h, i, j) {
    return fn(a)(b)(c)(d)(e)(f)(g)(h)(i)(j);
  };
};

exports.runFn0 = function (fn) {
  return fn();
};

exports.runFn1 = function (fn) {
  return function (a) {
    return fn(a);
  };
};

exports.runFn2 = function (fn) {
  return function (a) {
    return function (b) {
      return fn(a, b);
    };
  };
};

exports.runFn3 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return fn(a, b, c);
      };
    };
  };
};

exports.runFn4 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return fn(a, b, c, d);
        };
      };
    };
  };
};

exports.runFn5 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return fn(a, b, c, d, e);
          };
        };
      };
    };
  };
};

exports.runFn6 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return fn(a, b, c, d, e, f);
            };
          };
        };
      };
    };
  };
};

exports.runFn7 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return function (g) {
                return fn(a, b, c, d, e, f, g);
              };
            };
          };
        };
      };
    };
  };
};

exports.runFn8 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return function (g) {
                return function (h) {
                  return fn(a, b, c, d, e, f, g, h);
                };
              };
            };
          };
        };
      };
    };
  };
};

exports.runFn9 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return function (g) {
                return function (h) {
                  return function (i) {
                    return fn(a, b, c, d, e, f, g, h, i);
                  };
                };
              };
            };
          };
        };
      };
    };
  };
};

exports.runFn10 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return function (g) {
                return function (h) {
                  return function (i) {
                    return function (j) {
                      return fn(a, b, c, d, e, f, g, h, i, j);
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
};

},{}],96:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Data_Unit = require("../Data.Unit");
module.exports = {
    mkFn0: $foreign.mkFn0, 
    mkFn1: $foreign.mkFn1, 
    mkFn10: $foreign.mkFn10, 
    mkFn2: $foreign.mkFn2, 
    mkFn3: $foreign.mkFn3, 
    mkFn4: $foreign.mkFn4, 
    mkFn5: $foreign.mkFn5, 
    mkFn6: $foreign.mkFn6, 
    mkFn7: $foreign.mkFn7, 
    mkFn8: $foreign.mkFn8, 
    mkFn9: $foreign.mkFn9, 
    runFn0: $foreign.runFn0, 
    runFn1: $foreign.runFn1, 
    runFn10: $foreign.runFn10, 
    runFn2: $foreign.runFn2, 
    runFn3: $foreign.runFn3, 
    runFn4: $foreign.runFn4, 
    runFn5: $foreign.runFn5, 
    runFn6: $foreign.runFn6, 
    runFn7: $foreign.runFn7, 
    runFn8: $foreign.runFn8, 
    runFn9: $foreign.runFn9
};

},{"../Data.Unit":151,"./foreign":95}],97:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Category = require("../Control.Category");
var on = function (f) {
    return function (g) {
        return function (x) {
            return function (y) {
                return f(g(x))(g(y));
            };
        };
    };
};
var flip = function (f) {
    return function (b) {
        return function (a) {
            return f(a)(b);
        };
    };
};
var $$const = function (a) {
    return function (v) {
        return a;
    };
};
var applyFlipped = function (x) {
    return function (f) {
        return f(x);
    };
};
var apply = function (f) {
    return function (x) {
        return f(x);
    };
};
module.exports = {
    apply: apply, 
    applyFlipped: applyFlipped, 
    "const": $$const, 
    flip: flip, 
    on: on
};

},{"../Control.Category":14}],98:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Invariant = function (imap) {
    this.imap = imap;
};
var imapF = function (dictFunctor) {
    return function ($1) {
        return Data_Function["const"](Data_Functor.map(dictFunctor)($1));
    };
};
var invariantArray = new Invariant(imapF(Data_Functor.functorArray));
var invariantFn = new Invariant(imapF(Data_Functor.functorFn));
var imap = function (dict) {
    return dict.imap;
};
module.exports = {
    Invariant: Invariant, 
    imap: imap, 
    imapF: imapF, 
    invariantFn: invariantFn, 
    invariantArray: invariantArray
};

},{"../Control.Semigroupoid":53,"../Data.Function":97,"../Data.Functor":100}],99:[function(require,module,exports){
"use strict";

// module Data.Functor

exports.arrayMap = function (f) {
  return function (arr) {
    var l = arr.length;
    var result = new Array(l);
    for (var i = 0; i < l; i++) {
      result[i] = f(arr[i]);
    }
    return result;
  };
};

},{}],100:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Data_Function = require("../Data.Function");
var Data_Unit = require("../Data.Unit");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Functor = function (map) {
    this.map = map;
};
var map = function (dict) {
    return dict.map;
};
var mapFlipped = function (dictFunctor) {
    return function (fa) {
        return function (f) {
            return map(dictFunctor)(f)(fa);
        };
    };
};
var $$void = function (dictFunctor) {
    return map(dictFunctor)(Data_Function["const"](Data_Unit.unit));
};
var voidLeft = function (dictFunctor) {
    return function (f) {
        return function (x) {
            return map(dictFunctor)(Data_Function["const"](x))(f);
        };
    };
};
var voidRight = function (dictFunctor) {
    return function (x) {
        return map(dictFunctor)(Data_Function["const"](x));
    };
};
var functorFn = new Functor(Control_Semigroupoid.compose(Control_Semigroupoid.semigroupoidFn));
var functorArray = new Functor($foreign.arrayMap);
var flap = function (dictFunctor) {
    return function (ff) {
        return function (x) {
            return map(dictFunctor)(function (f) {
                return f(x);
            })(ff);
        };
    };
};
module.exports = {
    Functor: Functor, 
    flap: flap, 
    map: map, 
    mapFlipped: mapFlipped, 
    "void": $$void, 
    voidLeft: voidLeft, 
    voidRight: voidRight, 
    functorFn: functorFn, 
    functorArray: functorArray
};

},{"../Control.Semigroupoid":53,"../Data.Function":97,"../Data.Unit":151,"./foreign":99}],101:[function(require,module,exports){
"use strict";

// module Data.Generic

exports.zipAll = function (f) {
  return function (xs) {
    return function (ys) {
      var l = xs.length < ys.length ? xs.length : ys.length;
      for (var i = 0; i < l; i++) {
        if (!f(xs[i])(ys[i])) {
          return false;
        }
      }
      return true;
    };
  };
};

exports.zipCompare = function (f) {
  return function (xs) {
    return function (ys) {
      var i = 0;
      var xlen = xs.length;
      var ylen = ys.length;
      while (i < xlen && i < ylen) {
        var o = f(xs[i])(ys[i]);
        if (o !== 0) {
          return o;
        }
        i++;
      }
      if (xlen === ylen) {
        return 0;
      } else if (xlen > ylen) {
        return -1;
      } else {
        return 1;
      }
    };
  };
};

},{}],102:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Array = require("../Data.Array");
var Data_Either = require("../Data.Either");
var Data_Foldable = require("../Data.Foldable");
var Data_Maybe = require("../Data.Maybe");
var Data_String = require("../Data.String");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Type_Proxy = require("../Type.Proxy");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Functor = require("../Data.Functor");
var Data_Unit = require("../Data.Unit");
var Control_Apply = require("../Control.Apply");
var Control_Applicative = require("../Control.Applicative");
var Data_Ordering = require("../Data.Ordering");
var Data_Eq = require("../Data.Eq");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Ord = require("../Data.Ord");
var Data_Function = require("../Data.Function");
var Data_Show = require("../Data.Show");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Monoid = require("../Data.Monoid");
var Data_Ring = require("../Data.Ring");
var Data_Boolean = require("../Data.Boolean");
var Data_BooleanAlgebra = require("../Data.BooleanAlgebra");
var SProd = (function () {
    function SProd(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    SProd.create = function (value0) {
        return function (value1) {
            return new SProd(value0, value1);
        };
    };
    return SProd;
})();
var SRecord = (function () {
    function SRecord(value0) {
        this.value0 = value0;
    };
    SRecord.create = function (value0) {
        return new SRecord(value0);
    };
    return SRecord;
})();
var SNumber = (function () {
    function SNumber(value0) {
        this.value0 = value0;
    };
    SNumber.create = function (value0) {
        return new SNumber(value0);
    };
    return SNumber;
})();
var SBoolean = (function () {
    function SBoolean(value0) {
        this.value0 = value0;
    };
    SBoolean.create = function (value0) {
        return new SBoolean(value0);
    };
    return SBoolean;
})();
var SInt = (function () {
    function SInt(value0) {
        this.value0 = value0;
    };
    SInt.create = function (value0) {
        return new SInt(value0);
    };
    return SInt;
})();
var SString = (function () {
    function SString(value0) {
        this.value0 = value0;
    };
    SString.create = function (value0) {
        return new SString(value0);
    };
    return SString;
})();
var SChar = (function () {
    function SChar(value0) {
        this.value0 = value0;
    };
    SChar.create = function (value0) {
        return new SChar(value0);
    };
    return SChar;
})();
var SArray = (function () {
    function SArray(value0) {
        this.value0 = value0;
    };
    SArray.create = function (value0) {
        return new SArray(value0);
    };
    return SArray;
})();
var SUnit = (function () {
    function SUnit() {

    };
    SUnit.value = new SUnit();
    return SUnit;
})();
var SigProd = (function () {
    function SigProd(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    SigProd.create = function (value0) {
        return function (value1) {
            return new SigProd(value0, value1);
        };
    };
    return SigProd;
})();
var SigRecord = (function () {
    function SigRecord(value0) {
        this.value0 = value0;
    };
    SigRecord.create = function (value0) {
        return new SigRecord(value0);
    };
    return SigRecord;
})();
var SigNumber = (function () {
    function SigNumber() {

    };
    SigNumber.value = new SigNumber();
    return SigNumber;
})();
var SigBoolean = (function () {
    function SigBoolean() {

    };
    SigBoolean.value = new SigBoolean();
    return SigBoolean;
})();
var SigInt = (function () {
    function SigInt() {

    };
    SigInt.value = new SigInt();
    return SigInt;
})();
var SigString = (function () {
    function SigString() {

    };
    SigString.value = new SigString();
    return SigString;
})();
var SigChar = (function () {
    function SigChar() {

    };
    SigChar.value = new SigChar();
    return SigChar;
})();
var SigArray = (function () {
    function SigArray(value0) {
        this.value0 = value0;
    };
    SigArray.create = function (value0) {
        return new SigArray(value0);
    };
    return SigArray;
})();
var SigUnit = (function () {
    function SigUnit() {

    };
    SigUnit.value = new SigUnit();
    return SigUnit;
})();
var Generic = function (fromSpine, toSignature, toSpine) {
    this.fromSpine = fromSpine;
    this.toSignature = toSignature;
    this.toSpine = toSpine;
};
var toSpine = function (dict) {
    return dict.toSpine;
};
var toSignature = function (dict) {
    return dict.toSignature;
};
var showArray = function (f) {
    return function (xs) {
        return "[ " + (Data_Foldable.intercalate(Data_Foldable.foldableArray)(Data_Monoid.monoidString)(", ")(Data_Functor.map(Data_Functor.functorArray)(f)(xs)) + " ]");
    };
};
var orderingToInt = function (v) {
    if (v instanceof Data_Ordering.EQ) {
        return 0;
    };
    if (v instanceof Data_Ordering.LT) {
        return 1;
    };
    if (v instanceof Data_Ordering.GT) {
        return -1;
    };
    throw new Error("Failed pattern match at Data.Generic line 427, column 17 - line 430, column 10: " + [ v.constructor.name ]);
};
var genericUnit = new Generic(function (v) {
    if (v instanceof SUnit) {
        return new Data_Maybe.Just(Data_Unit.unit);
    };
    return Data_Maybe.Nothing.value;
}, function (v) {
    return SigUnit.value;
}, function (v) {
    return SUnit.value;
});
var genericString = new Generic(function (v) {
    if (v instanceof SString) {
        return new Data_Maybe.Just(v.value0);
    };
    return Data_Maybe.Nothing.value;
}, function (v) {
    return SigString.value;
}, SString.create);
var genericOrdering = new Generic(function (v) {
    if (v instanceof SProd && (v.value0 === "Data.Ordering.LT" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Data_Ordering.LT.value);
    };
    if (v instanceof SProd && (v.value0 === "Data.Ordering.EQ" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Data_Ordering.EQ.value);
    };
    if (v instanceof SProd && (v.value0 === "Data.Ordering.GT" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Data_Ordering.GT.value);
    };
    return Data_Maybe.Nothing.value;
}, function (v) {
    return new SigProd("Data.Ordering.Ordering", [ {
        sigConstructor: "Data.Ordering.LT", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Ordering.EQ", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Ordering.GT", 
        sigValues: [  ]
    } ]);
}, function (v) {
    if (v instanceof Data_Ordering.LT) {
        return new SProd("Data.Ordering.LT", [  ]);
    };
    if (v instanceof Data_Ordering.EQ) {
        return new SProd("Data.Ordering.EQ", [  ]);
    };
    if (v instanceof Data_Ordering.GT) {
        return new SProd("Data.Ordering.GT", [  ]);
    };
    throw new Error("Failed pattern match at Data.Generic line 150, column 13 - line 153, column 38: " + [ v.constructor.name ]);
});
var genericNumber = new Generic(function (v) {
    if (v instanceof SNumber) {
        return new Data_Maybe.Just(v.value0);
    };
    return Data_Maybe.Nothing.value;
}, function (v) {
    return SigNumber.value;
}, SNumber.create);
var genericInt = new Generic(function (v) {
    if (v instanceof SInt) {
        return new Data_Maybe.Just(v.value0);
    };
    return Data_Maybe.Nothing.value;
}, function (v) {
    return SigInt.value;
}, SInt.create);
var genericChar = new Generic(function (v) {
    if (v instanceof SChar) {
        return new Data_Maybe.Just(v.value0);
    };
    return Data_Maybe.Nothing.value;
}, function (v) {
    return SigChar.value;
}, SChar.create);
var genericBool = new Generic(function (v) {
    if (v instanceof SBoolean) {
        return new Data_Maybe.Just(v.value0);
    };
    return Data_Maybe.Nothing.value;
}, function (v) {
    return SigBoolean.value;
}, SBoolean.create);
var fromSpine = function (dict) {
    return dict.fromSpine;
};
var force = function (f) {
    return f(Data_Unit.unit);
};
var genericArray = function (dictGeneric) {
    return new Generic(function (v) {
        if (v instanceof SArray) {
            return Data_Traversable.traverse(Data_Traversable.traversableArray)(Data_Maybe.applicativeMaybe)(function ($237) {
                return fromSpine(dictGeneric)(force($237));
            })(v.value0);
        };
        return Data_Maybe.Nothing.value;
    }, function (x) {
        var lowerProxy = function (v) {
            return (Type_Proxy["Proxy"]).value;
        };
        return new SigArray(function (v) {
            return toSignature(dictGeneric)(lowerProxy(x));
        });
    }, function ($238) {
        return SArray.create(Data_Functor.map(Data_Functor.functorArray)(function (x) {
            return function (v) {
                return toSpine(dictGeneric)(x);
            };
        })($238));
    });
};
var genericEither = function (dictGeneric) {
    return function (dictGeneric1) {
        return new Generic(function (v) {
            if (v instanceof SProd && (v.value0 === "Data.Either.Left" && v.value1.length === 1)) {
                return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Either.Left.create)(fromSpine(dictGeneric)(force(v.value1[0])));
            };
            if (v instanceof SProd && (v.value0 === "Data.Either.Right" && v.value1.length === 1)) {
                return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Either.Right.create)(fromSpine(dictGeneric1)(force(v.value1[0])));
            };
            return Data_Maybe.Nothing.value;
        }, function (x) {
            var rproxy = function (v) {
                return (Type_Proxy["Proxy"]).value;
            };
            var lproxy = function (v) {
                return (Type_Proxy["Proxy"]).value;
            };
            return new SigProd("Data.Either.Either", [ {
                sigConstructor: "Data.Either.Left", 
                sigValues: [ function (v) {
                    return toSignature(dictGeneric)(lproxy(x));
                } ]
            }, {
                sigConstructor: "Data.Either.Right", 
                sigValues: [ function (v) {
                    return toSignature(dictGeneric1)(rproxy(x));
                } ]
            } ]);
        }, function (v) {
            if (v instanceof Data_Either.Left) {
                return new SProd("Data.Either.Left", [ function (v1) {
                    return toSpine(dictGeneric)(v.value0);
                } ]);
            };
            if (v instanceof Data_Either.Right) {
                return new SProd("Data.Either.Right", [ function (v1) {
                    return toSpine(dictGeneric1)(v.value0);
                } ]);
            };
            throw new Error("Failed pattern match at Data.Generic line 128, column 3 - line 128, column 64: " + [ v.constructor.name ]);
        });
    };
};
var genericMaybe = function (dictGeneric) {
    return new Generic(function (v) {
        if (v instanceof SProd && (v.value0 === "Data.Maybe.Just" && v.value1.length === 1)) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Maybe.Just.create)(fromSpine(dictGeneric)(force(v.value1[0])));
        };
        if (v instanceof SProd && (v.value0 === "Data.Maybe.Nothing" && v.value1.length === 0)) {
            return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(Data_Maybe.Nothing.value);
        };
        return Data_Maybe.Nothing.value;
    }, function (x) {
        var mbProxy = function (v) {
            return (Type_Proxy["Proxy"]).value;
        };
        return new SigProd("Data.Maybe.Maybe", [ {
            sigConstructor: "Data.Maybe.Just", 
            sigValues: [ function (v) {
                return toSignature(dictGeneric)(mbProxy(x));
            } ]
        }, {
            sigConstructor: "Data.Maybe.Nothing", 
            sigValues: [  ]
        } ]);
    }, function (v) {
        if (v instanceof Data_Maybe.Just) {
            return new SProd("Data.Maybe.Just", [ function (v1) {
                return toSpine(dictGeneric)(v.value0);
            } ]);
        };
        if (v instanceof Data_Maybe.Nothing) {
            return new SProd("Data.Maybe.Nothing", [  ]);
        };
        throw new Error("Failed pattern match at Data.Generic line 108, column 3 - line 108, column 63: " + [ v.constructor.name ]);
    });
};
var genericShowPrec = function (v) {
    return function (v1) {
        if (v1 instanceof SProd) {
            if (Data_Array["null"](v1.value1)) {
                return v1.value0;
            };
            if (Data_Boolean.otherwise) {
                var showParen = function (v2) {
                    return function (x) {
                        if (!v2) {
                            return x;
                        };
                        if (v2) {
                            return "(" + (x + ")");
                        };
                        throw new Error("Failed pattern match at Data.Generic line 356, column 7 - line 356, column 28: " + [ v2.constructor.name, x.constructor.name ]);
                    };
                };
                return Data_Function.apply(showParen(v > 10))(v1.value0 + (" " + Data_String.joinWith(" ")(Data_Functor.map(Data_Functor.functorArray)(function (x) {
                    return genericShowPrec(11)(force(x));
                })(v1.value1))));
            };
        };
        if (v1 instanceof SRecord) {
            var showLabelPart = function (x) {
                return x.recLabel + (": " + genericShowPrec(0)(force(x.recValue)));
            };
            return "{" + (Data_String.joinWith(", ")(Data_Functor.map(Data_Functor.functorArray)(showLabelPart)(v1.value0)) + "}");
        };
        if (v1 instanceof SBoolean) {
            return Data_Show.show(Data_Show.showBoolean)(v1.value0);
        };
        if (v1 instanceof SInt) {
            return Data_Show.show(Data_Show.showInt)(v1.value0);
        };
        if (v1 instanceof SNumber) {
            return Data_Show.show(Data_Show.showNumber)(v1.value0);
        };
        if (v1 instanceof SString) {
            return Data_Show.show(Data_Show.showString)(v1.value0);
        };
        if (v1 instanceof SChar) {
            return Data_Show.show(Data_Show.showChar)(v1.value0);
        };
        if (v1 instanceof SArray) {
            return "[" + (Data_String.joinWith(", ")(Data_Functor.map(Data_Functor.functorArray)(function (x) {
                return genericShowPrec(0)(force(x));
            })(v1.value0)) + "]");
        };
        if (v1 instanceof SUnit) {
            return "unit";
        };
        throw new Error("Failed pattern match at Data.Generic line 350, column 1 - line 358, column 1: " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var gShow = function (dictGeneric) {
    return function ($239) {
        return genericShowPrec(0)(toSpine(dictGeneric)($239));
    };
};
var genericTuple = function (dictGeneric) {
    return function (dictGeneric1) {
        return new Generic(function (v) {
            if (v instanceof SProd && (v.value0 === "Data.Tuple.Tuple" && v.value1.length === 2)) {
                return Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Tuple.Tuple.create)(fromSpine(dictGeneric)(force(v.value1[0]))))(fromSpine(dictGeneric1)(force(v.value1[1])));
            };
            return Data_Maybe.Nothing.value;
        }, function (x) {
            var sndProxy = function (v) {
                return (Type_Proxy["Proxy"]).value;
            };
            var fstProxy = function (v) {
                return (Type_Proxy["Proxy"]).value;
            };
            return new SigProd("Data.Tuple.Tuple", [ {
                sigConstructor: "Data.Tuple.Tuple", 
                sigValues: [ function (v) {
                    return toSignature(dictGeneric)(fstProxy(x));
                }, function (v) {
                    return toSignature(dictGeneric1)(sndProxy(x));
                } ]
            } ]);
        }, function (v) {
            return new SProd("Data.Tuple.Tuple", [ function (v1) {
                return toSpine(dictGeneric)(v.value0);
            }, function (v1) {
                return toSpine(dictGeneric1)(v.value1);
            } ]);
        });
    };
};
var isValidSpine = function (v) {
    return function (v1) {
        if (v instanceof SigBoolean && v1 instanceof SBoolean) {
            return true;
        };
        if (v instanceof SigNumber && v1 instanceof SNumber) {
            return true;
        };
        if (v instanceof SigInt && v1 instanceof SInt) {
            return true;
        };
        if (v instanceof SigString && v1 instanceof SString) {
            return true;
        };
        if (v instanceof SigChar && v1 instanceof SChar) {
            return true;
        };
        if (v instanceof SigArray && v1 instanceof SArray) {
            return Data_Foldable.all(Data_Foldable.foldableArray)(Data_BooleanAlgebra.booleanAlgebraBoolean)(function ($240) {
                return isValidSpine(force(v.value0))(force($240));
            })(v1.value0);
        };
        if (v instanceof SigProd && v1 instanceof SProd) {
            var $147 = Data_Foldable.find(Data_Foldable.foldableArray)(function (alt) {
                return alt.sigConstructor === v1.value0;
            })(v.value1);
            if ($147 instanceof Data_Maybe.Nothing) {
                return false;
            };
            if ($147 instanceof Data_Maybe.Just) {
                return Data_Function.apply(Data_Foldable.and(Data_Foldable.foldableArray)(Data_BooleanAlgebra.booleanAlgebraBoolean))(Data_Array.zipWith(function (sig) {
                    return function (spine) {
                        return isValidSpine(force(sig))(force(spine));
                    };
                })($147.value0.sigValues)(v1.value1));
            };
            throw new Error("Failed pattern match at Data.Generic line 327, column 3 - line 333, column 15: " + [ $147.constructor.name ]);
        };
        if (v instanceof SigRecord && v1 instanceof SRecord) {
            return Data_Function.apply(Data_Foldable.and(Data_Foldable.foldableArray)(Data_BooleanAlgebra.booleanAlgebraBoolean))(Data_Array.zipWith(function (sig) {
                return function (val) {
                    return isValidSpine(force(sig.recValue))(force(val.recValue));
                };
            })(Data_Array.sortBy(function (a) {
                return function (b) {
                    return Data_Ord.compare(Data_Ord.ordString)(a.recLabel)(b.recLabel);
                };
            })(v.value0))(Data_Array.sortBy(function (a) {
                return function (b) {
                    return Data_Ord.compare(Data_Ord.ordString)(a.recLabel)(b.recLabel);
                };
            })(v1.value0)));
        };
        if (v instanceof SigUnit && v1 instanceof SUnit) {
            return true;
        };
        return false;
    };
};
var showSignature = function (sig) {
    var needsParen = function (s) {
        if (s instanceof SigProd) {
            return true;
        };
        if (s instanceof SigRecord) {
            return true;
        };
        if (s instanceof SigNumber) {
            return false;
        };
        if (s instanceof SigBoolean) {
            return false;
        };
        if (s instanceof SigInt) {
            return false;
        };
        if (s instanceof SigString) {
            return false;
        };
        if (s instanceof SigChar) {
            return false;
        };
        if (s instanceof SigArray) {
            return true;
        };
        if (s instanceof SigUnit) {
            return false;
        };
        throw new Error("Failed pattern match at Data.Generic line 293, column 18 - line 302, column 21: " + [ s.constructor.name ]);
    };
    var paren = function (s) {
        if (needsParen(s)) {
            return "(" + (showSignature(s) + ")");
        };
        if (Data_Boolean.otherwise) {
            return showSignature(s);
        };
        throw new Error("Failed pattern match at Data.Generic line 275, column 1 - line 302, column 21: " + [ s.constructor.name ]);
    };
    return Data_Function.apply(Data_Foldable.fold(Data_Foldable.foldableArray)(Data_Monoid.monoidString))((function () {
        if (sig instanceof SigProd) {
            return [ "SigProd ", Data_Show.show(Data_Show.showString)(sig.value0), " ", showArray(showDataConstructor)(sig.value1) ];
        };
        if (sig instanceof SigRecord) {
            return [ "SigRecord ", showArray(showLabel)(sig.value0) ];
        };
        if (sig instanceof SigNumber) {
            return [ "SigNumber" ];
        };
        if (sig instanceof SigBoolean) {
            return [ "SigBoolean" ];
        };
        if (sig instanceof SigInt) {
            return [ "SigInt" ];
        };
        if (sig instanceof SigString) {
            return [ "SigString" ];
        };
        if (sig instanceof SigChar) {
            return [ "SigChar" ];
        };
        if (sig instanceof SigArray) {
            return [ "SigArray ", paren(force(sig.value0)) ];
        };
        if (sig instanceof SigUnit) {
            return [ "SigUnit" ];
        };
        throw new Error("Failed pattern match at Data.Generic line 276, column 3 - line 286, column 27: " + [ sig.constructor.name ]);
    })());
};
var showLabel = function (l) {
    return "{ recLabel: " + (Data_Show.show(Data_Show.showString)(l.recLabel) + (", recValue: " + (showSignature(force(l.recValue)) + " }")));
};
var showDataConstructor = function (dc) {
    return "{ sigConstructor: " + (Data_Show.show(Data_Show.showString)(dc.sigConstructor) + (", sigValues: " + (showArray(function ($241) {
        return showSignature(force($241));
    })(dc.sigValues) + "}")));
};
var showGenericSignature = new Data_Show.Show(showSignature);
var eqThunk = function (dictEq) {
    return function (x) {
        return function (y) {
            return Data_Eq.eq(dictEq)(force(x))(force(y));
        };
    };
};
var eqRecordSigs = function (dictEq) {
    return function (arr1) {
        return function (arr2) {
            var labelCompare = function (r1) {
                return function (r2) {
                    return Data_Ord.compare(Data_Ord.ordString)(r1.recLabel)(r2.recLabel);
                };
            };
            var sorted1 = Data_Array.sortBy(labelCompare)(arr1);
            var sorted2 = Data_Array.sortBy(labelCompare)(arr2);
            var doCmp = function (x) {
                return function (y) {
                    return x.recLabel === y.recLabel && Data_Eq.eq(dictEq)(force(x.recValue))(force(y.recValue));
                };
            };
            return Data_Array.length(arr1) === Data_Array.length(arr2) && $foreign.zipAll(doCmp)(sorted1)(sorted2);
        };
    };
};
var eqGenericSpine = new Data_Eq.Eq(function (v) {
    return function (v1) {
        if (v instanceof SProd && v1 instanceof SProd) {
            return v.value0 === v1.value0 && (Data_Array.length(v.value1) === Data_Array.length(v1.value1) && $foreign.zipAll(eqThunk(eqGenericSpine))(v.value1)(v1.value1));
        };
        if (v instanceof SRecord && v1 instanceof SRecord) {
            return eqRecordSigs(eqGenericSpine)(v.value0)(v1.value0);
        };
        if (v instanceof SNumber && v1 instanceof SNumber) {
            return v.value0 === v1.value0;
        };
        if (v instanceof SBoolean && v1 instanceof SBoolean) {
            return v.value0 === v1.value0;
        };
        if (v instanceof SInt && v1 instanceof SInt) {
            return v.value0 === v1.value0;
        };
        if (v instanceof SString && v1 instanceof SString) {
            return v.value0 === v1.value0;
        };
        if (v instanceof SChar && v1 instanceof SChar) {
            return v.value0 === v1.value0;
        };
        if (v instanceof SArray && v1 instanceof SArray) {
            return Data_Array.length(v.value0) === Data_Array.length(v1.value0) && $foreign.zipAll(eqThunk(eqGenericSpine))(v.value0)(v1.value0);
        };
        if (v instanceof SUnit && v1 instanceof SUnit) {
            return true;
        };
        return false;
    };
});
var gEq = function (dictGeneric) {
    return function (x) {
        return function (y) {
            return Data_Eq.eq(eqGenericSpine)(toSpine(dictGeneric)(x))(toSpine(dictGeneric)(y));
        };
    };
};
var eqGenericSignature = new Data_Eq.Eq(function (v) {
    return function (v1) {
        if (v instanceof SigProd && v1 instanceof SigProd) {
            return v.value0 === v1.value0 && (Data_Array.length(v.value1) === Data_Array.length(v1.value1) && $foreign.zipAll(eqDataConstructor)(v.value1)(v1.value1));
        };
        if (v instanceof SigRecord && v1 instanceof SigRecord) {
            return eqRecordSigs(eqGenericSignature)(v.value0)(v1.value0);
        };
        if (v instanceof SigNumber && v1 instanceof SigNumber) {
            return true;
        };
        if (v instanceof SigBoolean && v1 instanceof SigBoolean) {
            return true;
        };
        if (v instanceof SigInt && v1 instanceof SigInt) {
            return true;
        };
        if (v instanceof SigString && v1 instanceof SigString) {
            return true;
        };
        if (v instanceof SigChar && v1 instanceof SigChar) {
            return true;
        };
        if (v instanceof SigArray && v1 instanceof SigArray) {
            return eqThunk(eqGenericSignature)(v.value0)(v1.value0);
        };
        if (v instanceof SigUnit && v1 instanceof SigUnit) {
            return true;
        };
        return false;
    };
});
var eqDataConstructor = function (p1) {
    return function (p2) {
        return p1.sigConstructor === p2.sigConstructor && $foreign.zipAll(eqThunk(eqGenericSignature))(p1.sigValues)(p2.sigValues);
    };
};
var compareThunk = function (dictOrd) {
    return function (x) {
        return function (y) {
            return Data_Function.apply(orderingToInt)(Data_Ord.compare(dictOrd)(force(x))(force(y)));
        };
    };
};
var ordGenericSpine = new Data_Ord.Ord(function () {
    return eqGenericSpine;
}, function (v) {
    return function (v1) {
        if (v instanceof SProd && v1 instanceof SProd) {
            var $199 = Data_Ord.compare(Data_Ord.ordString)(v.value0)(v1.value0);
            if ($199 instanceof Data_Ordering.EQ) {
                return Data_Function.apply(Data_Ord.compare(Data_Ord.ordInt)(0))($foreign.zipCompare(compareThunk(ordGenericSpine))(v.value1)(v1.value1));
            };
            return $199;
        };
        if (v instanceof SProd) {
            return Data_Ordering.LT.value;
        };
        if (v1 instanceof SProd) {
            return Data_Ordering.GT.value;
        };
        if (v instanceof SRecord && v1 instanceof SRecord) {
            var go = function (x) {
                return function (y) {
                    var $208 = Data_Ord.compare(Data_Ord.ordString)(x.recLabel)(y.recLabel);
                    if ($208 instanceof Data_Ordering.EQ) {
                        return Data_Function.apply(orderingToInt)(Data_Ord.compare(ordGenericSpine)(force(x.recValue))(force(y.recValue)));
                    };
                    return orderingToInt($208);
                };
            };
            return Data_Function.apply(Data_Ord.compare(Data_Ord.ordInt)(0))($foreign.zipCompare(go)(v.value0)(v1.value0));
        };
        if (v instanceof SRecord) {
            return Data_Ordering.LT.value;
        };
        if (v1 instanceof SRecord) {
            return Data_Ordering.GT.value;
        };
        if (v instanceof SInt && v1 instanceof SInt) {
            return Data_Ord.compare(Data_Ord.ordInt)(v.value0)(v1.value0);
        };
        if (v instanceof SInt) {
            return Data_Ordering.LT.value;
        };
        if (v1 instanceof SInt) {
            return Data_Ordering.GT.value;
        };
        if (v instanceof SBoolean && v1 instanceof SBoolean) {
            return Data_Ord.compare(Data_Ord.ordBoolean)(v.value0)(v1.value0);
        };
        if (v instanceof SBoolean) {
            return Data_Ordering.LT.value;
        };
        if (v1 instanceof SBoolean) {
            return Data_Ordering.GT.value;
        };
        if (v instanceof SNumber && v1 instanceof SNumber) {
            return Data_Ord.compare(Data_Ord.ordNumber)(v.value0)(v1.value0);
        };
        if (v instanceof SNumber) {
            return Data_Ordering.LT.value;
        };
        if (v1 instanceof SNumber) {
            return Data_Ordering.GT.value;
        };
        if (v instanceof SString && v1 instanceof SString) {
            return Data_Ord.compare(Data_Ord.ordString)(v.value0)(v1.value0);
        };
        if (v instanceof SString) {
            return Data_Ordering.LT.value;
        };
        if (v1 instanceof SString) {
            return Data_Ordering.GT.value;
        };
        if (v instanceof SChar && v1 instanceof SChar) {
            return Data_Ord.compare(Data_Ord.ordChar)(v.value0)(v1.value0);
        };
        if (v instanceof SChar) {
            return Data_Ordering.LT.value;
        };
        if (v1 instanceof SChar) {
            return Data_Ordering.GT.value;
        };
        if (v instanceof SArray && v1 instanceof SArray) {
            return Data_Function.apply(Data_Ord.compare(Data_Ord.ordInt)(0))($foreign.zipCompare(compareThunk(ordGenericSpine))(v.value0)(v1.value0));
        };
        if (v instanceof SArray) {
            return Data_Ordering.LT.value;
        };
        if (v1 instanceof SArray) {
            return Data_Ordering.GT.value;
        };
        if (v instanceof SUnit && v1 instanceof SUnit) {
            return Data_Ordering.EQ.value;
        };
        throw new Error("Failed pattern match at Data.Generic line 194, column 3 - line 197, column 15: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var gCompare = function (dictGeneric) {
    return function (x) {
        return function (y) {
            return Data_Ord.compare(ordGenericSpine)(toSpine(dictGeneric)(x))(toSpine(dictGeneric)(y));
        };
    };
};
module.exports = {
    SigProd: SigProd, 
    SigRecord: SigRecord, 
    SigNumber: SigNumber, 
    SigBoolean: SigBoolean, 
    SigInt: SigInt, 
    SigString: SigString, 
    SigChar: SigChar, 
    SigArray: SigArray, 
    SigUnit: SigUnit, 
    SProd: SProd, 
    SRecord: SRecord, 
    SNumber: SNumber, 
    SBoolean: SBoolean, 
    SInt: SInt, 
    SString: SString, 
    SChar: SChar, 
    SArray: SArray, 
    SUnit: SUnit, 
    Generic: Generic, 
    fromSpine: fromSpine, 
    gCompare: gCompare, 
    gEq: gEq, 
    gShow: gShow, 
    isValidSpine: isValidSpine, 
    showDataConstructor: showDataConstructor, 
    showSignature: showSignature, 
    toSignature: toSignature, 
    toSpine: toSpine, 
    genericNumber: genericNumber, 
    genericInt: genericInt, 
    genericString: genericString, 
    genericChar: genericChar, 
    genericBool: genericBool, 
    genericArray: genericArray, 
    genericUnit: genericUnit, 
    genericTuple: genericTuple, 
    genericMaybe: genericMaybe, 
    genericEither: genericEither, 
    genericOrdering: genericOrdering, 
    eqGenericSpine: eqGenericSpine, 
    ordGenericSpine: ordGenericSpine, 
    eqGenericSignature: eqGenericSignature, 
    showGenericSignature: showGenericSignature
};

},{"../Control.Applicative":7,"../Control.Apply":9,"../Control.Semigroupoid":53,"../Data.Array":67,"../Data.Boolean":71,"../Data.BooleanAlgebra":72,"../Data.Either":77,"../Data.Eq":79,"../Data.Foldable":84,"../Data.Function":97,"../Data.Functor":100,"../Data.HeytingAlgebra":104,"../Data.Maybe":113,"../Data.Monoid":120,"../Data.Ord":127,"../Data.Ordering":128,"../Data.Ring":130,"../Data.Semigroup":132,"../Data.Show":136,"../Data.String":144,"../Data.Traversable":146,"../Data.Tuple":147,"../Data.Unit":151,"../Prelude":162,"../Type.Proxy":163,"./foreign":101}],103:[function(require,module,exports){
"use strict";

// module Data.HeytingAlgebra

exports.boolConj = function (b1) {
  return function (b2) {
    return b1 && b2;
  };
};

exports.boolDisj = function (b1) {
  return function (b2) {
    return b1 || b2;
  };
};

exports.boolNot = function (b) {
  return !b;
};

},{}],104:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Data_Unit = require("../Data.Unit");
var HeytingAlgebra = function (conj, disj, ff, implies, not, tt) {
    this.conj = conj;
    this.disj = disj;
    this.ff = ff;
    this.implies = implies;
    this.not = not;
    this.tt = tt;
};
var tt = function (dict) {
    return dict.tt;
};
var not = function (dict) {
    return dict.not;
};
var implies = function (dict) {
    return dict.implies;
};
var heytingAlgebraUnit = new HeytingAlgebra(function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
}, function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
}, Data_Unit.unit, function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
}, function (v) {
    return Data_Unit.unit;
}, Data_Unit.unit);
var ff = function (dict) {
    return dict.ff;
};
var disj = function (dict) {
    return dict.disj;
};
var heytingAlgebraBoolean = new HeytingAlgebra($foreign.boolConj, $foreign.boolDisj, false, function (a) {
    return function (b) {
        return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a))(b);
    };
}, $foreign.boolNot, true);
var conj = function (dict) {
    return dict.conj;
};
var heytingAlgebraFunction = function (dictHeytingAlgebra) {
    return new HeytingAlgebra(function (f) {
        return function (g) {
            return function (a) {
                return conj(dictHeytingAlgebra)(f(a))(g(a));
            };
        };
    }, function (f) {
        return function (g) {
            return function (a) {
                return disj(dictHeytingAlgebra)(f(a))(g(a));
            };
        };
    }, function (v) {
        return ff(dictHeytingAlgebra);
    }, function (f) {
        return function (g) {
            return function (a) {
                return implies(dictHeytingAlgebra)(f(a))(g(a));
            };
        };
    }, function (f) {
        return function (a) {
            return not(dictHeytingAlgebra)(f(a));
        };
    }, function (v) {
        return tt(dictHeytingAlgebra);
    });
};
module.exports = {
    HeytingAlgebra: HeytingAlgebra, 
    conj: conj, 
    disj: disj, 
    ff: ff, 
    implies: implies, 
    not: not, 
    tt: tt, 
    heytingAlgebraBoolean: heytingAlgebraBoolean, 
    heytingAlgebraUnit: heytingAlgebraUnit, 
    heytingAlgebraFunction: heytingAlgebraFunction
};

},{"../Data.Unit":151,"./foreign":103}],105:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Comonad = require("../Control.Comonad");
var Control_Extend = require("../Control.Extend");
var Control_Monad = require("../Control.Monad");
var Data_BooleanAlgebra = require("../Data.BooleanAlgebra");
var Data_Bounded = require("../Data.Bounded");
var Data_CommutativeRing = require("../Data.CommutativeRing");
var Data_Eq = require("../Data.Eq");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Field = require("../Data.Field");
var Data_Foldable = require("../Data.Foldable");
var Data_Functor = require("../Data.Functor");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Monoid = require("../Data.Monoid");
var Data_Ord = require("../Data.Ord");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_Traversable = require("../Data.Traversable");
var Identity = function (x) {
    return x;
};
var showIdentity = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Identity " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var semiringIdentity = function (dictSemiring) {
    return new Data_Semiring.Semiring(function (v) {
        return function (v1) {
            return Data_Semiring.add(dictSemiring)(v)(v1);
        };
    }, function (v) {
        return function (v1) {
            return Data_Semiring.mul(dictSemiring)(v)(v1);
        };
    }, Data_Semiring.one(dictSemiring), Data_Semiring.zero(dictSemiring));
};
var semigroupIdenity = function (dictSemigroup) {
    return new Data_Semigroup.Semigroup(function (v) {
        return function (v1) {
            return Data_Semigroup.append(dictSemigroup)(v)(v1);
        };
    });
};
var runIdentity = function (v) {
    return v;
};
var ringIdentity = function (dictRing) {
    return new Data_Ring.Ring(function () {
        return semiringIdentity(dictRing["__superclass_Data.Semiring.Semiring_0"]());
    }, function (v) {
        return function (v1) {
            return Data_Ring.sub(dictRing)(v)(v1);
        };
    });
};
var monoidIdentity = function (dictMonoid) {
    return new Data_Monoid.Monoid(function () {
        return semigroupIdenity(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]());
    }, Data_Monoid.mempty(dictMonoid));
};
var heytingAlgebraIdentity = function (dictHeytingAlgebra) {
    return new Data_HeytingAlgebra.HeytingAlgebra(function (v) {
        return function (v1) {
            return Data_HeytingAlgebra.conj(dictHeytingAlgebra)(v)(v1);
        };
    }, function (v) {
        return function (v1) {
            return Data_HeytingAlgebra.disj(dictHeytingAlgebra)(v)(v1);
        };
    }, Data_HeytingAlgebra.ff(dictHeytingAlgebra), function (v) {
        return function (v1) {
            return Data_HeytingAlgebra.implies(dictHeytingAlgebra)(v)(v1);
        };
    }, function (v) {
        return Data_HeytingAlgebra.not(dictHeytingAlgebra)(v);
    }, Data_HeytingAlgebra.tt(dictHeytingAlgebra));
};
var functorIdentity = new Data_Functor.Functor(function (f) {
    return function (v) {
        return f(v);
    };
});
var invariantIdentity = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorIdentity));
var foldableIdentity = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var traversableIdentity = new Data_Traversable.Traversable(function () {
    return foldableIdentity;
}, function () {
    return functorIdentity;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Identity)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Identity)(f(v));
        };
    };
});
var extendIdentity = new Control_Extend.Extend(function () {
    return functorIdentity;
}, function (f) {
    return function (m) {
        return f(m);
    };
});
var eqIdentity = function (dictEq) {
    return new Data_Eq.Eq(function (v) {
        return function (v1) {
            return Data_Eq.eq(dictEq)(v)(v1);
        };
    });
};
var ordIdentity = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqIdentity(dictOrd["__superclass_Data.Eq.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Data_Ord.compare(dictOrd)(v)(v1);
        };
    });
};
var comonadIdentity = new Control_Comonad.Comonad(function () {
    return extendIdentity;
}, function (v) {
    return v;
});
var commutativeRingIdentity = function (dictCommutativeRing) {
    return new Data_CommutativeRing.CommutativeRing(function () {
        return ringIdentity(dictCommutativeRing["__superclass_Data.Ring.Ring_0"]());
    });
};
var euclideanRingIdentity = function (dictEuclideanRing) {
    return new Data_EuclideanRing.EuclideanRing(function () {
        return commutativeRingIdentity(dictEuclideanRing["__superclass_Data.CommutativeRing.CommutativeRing_0"]());
    }, function (v) {
        return Data_EuclideanRing.degree(dictEuclideanRing)(v);
    }, function (v) {
        return function (v1) {
            return Data_EuclideanRing.div(dictEuclideanRing)(v)(v1);
        };
    }, function (v) {
        return function (v1) {
            return Data_EuclideanRing.mod(dictEuclideanRing)(v)(v1);
        };
    });
};
var fieldIdentity = function (dictField) {
    return new Data_Field.Field(function () {
        return euclideanRingIdentity(dictField["__superclass_Data.EuclideanRing.EuclideanRing_0"]());
    });
};
var boundedIdentity = function (dictBounded) {
    return new Data_Bounded.Bounded(function () {
        return ordIdentity(dictBounded["__superclass_Data.Ord.Ord_0"]());
    }, Data_Bounded.bottom(dictBounded), Data_Bounded.top(dictBounded));
};
var booleanAlgebraIdentity = function (dictBooleanAlgebra) {
    return new Data_BooleanAlgebra.BooleanAlgebra(function () {
        return heytingAlgebraIdentity(dictBooleanAlgebra["__superclass_Data.HeytingAlgebra.HeytingAlgebra_0"]());
    });
};
var applyIdentity = new Control_Apply.Apply(function () {
    return functorIdentity;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindIdentity = new Control_Bind.Bind(function () {
    return applyIdentity;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeIdentity = new Control_Applicative.Applicative(function () {
    return applyIdentity;
}, Identity);
var monadIdentity = new Control_Monad.Monad(function () {
    return applicativeIdentity;
}, function () {
    return bindIdentity;
});
var altIdentity = new Control_Alt.Alt(function () {
    return functorIdentity;
}, function (x) {
    return function (v) {
        return x;
    };
});
module.exports = {
    Identity: Identity, 
    runIdentity: runIdentity, 
    eqIdentity: eqIdentity, 
    ordIdentity: ordIdentity, 
    boundedIdentity: boundedIdentity, 
    heytingAlgebraIdentity: heytingAlgebraIdentity, 
    booleanAlgebraIdentity: booleanAlgebraIdentity, 
    semigroupIdenity: semigroupIdenity, 
    monoidIdentity: monoidIdentity, 
    semiringIdentity: semiringIdentity, 
    euclideanRingIdentity: euclideanRingIdentity, 
    ringIdentity: ringIdentity, 
    commutativeRingIdentity: commutativeRingIdentity, 
    fieldIdentity: fieldIdentity, 
    showIdentity: showIdentity, 
    functorIdentity: functorIdentity, 
    invariantIdentity: invariantIdentity, 
    altIdentity: altIdentity, 
    applyIdentity: applyIdentity, 
    applicativeIdentity: applicativeIdentity, 
    bindIdentity: bindIdentity, 
    monadIdentity: monadIdentity, 
    extendIdentity: extendIdentity, 
    comonadIdentity: comonadIdentity, 
    foldableIdentity: foldableIdentity, 
    traversableIdentity: traversableIdentity
};

},{"../Control.Alt":5,"../Control.Applicative":7,"../Control.Apply":9,"../Control.Bind":13,"../Control.Comonad":15,"../Control.Extend":16,"../Control.Monad":48,"../Data.BooleanAlgebra":72,"../Data.Bounded":74,"../Data.CommutativeRing":75,"../Data.Eq":79,"../Data.EuclideanRing":81,"../Data.Field":82,"../Data.Foldable":84,"../Data.Functor":100,"../Data.Functor.Invariant":98,"../Data.HeytingAlgebra":104,"../Data.Monoid":120,"../Data.Ord":127,"../Data.Ring":130,"../Data.Semigroup":132,"../Data.Semiring":134,"../Data.Show":136,"../Data.Traversable":146}],106:[function(require,module,exports){
"use strict";

// module Data.Int.Bits

exports.and = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 & n2;
  };
};

exports.or = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 | n2;
  };
};

exports.xor = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 ^ n2;
  };
};

exports.shl = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 << n2;
  };
};

exports.shr = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 >> n2;
  };
};

exports.zshr = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 >>> n2;
  };
};

exports.complement = function (n) {
  /* jshint bitwise: false */
  return ~n;
};

},{}],107:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
module.exports = {
    and: $foreign.and, 
    complement: $foreign.complement, 
    or: $foreign.or, 
    shl: $foreign.shl, 
    shr: $foreign.shr, 
    xor: $foreign.xor, 
    zshr: $foreign.zshr
};

},{"./foreign":106}],108:[function(require,module,exports){
"use strict";

// module Data.Int

exports.fromNumberImpl = function (just) {
  return function (nothing) {
    return function (n) {
      /* jshint bitwise: false */
      return (n | 0) === n ? just(n) : nothing;
    };
  };
};

exports.toNumber = function (n) {
  return n;
};

exports.fromStringAsImpl = function (just) {
  return function (nothing) {
    return function (radix) {
      var digits;
      if (radix < 11) {
        digits = "[0-" + (radix - 1).toString() + "]";
      } else if (radix === 11) {
        digits = "[0-9a]";
      } else {
        digits = "[0-9a-" + String.fromCharCode(86 + radix) + "]";
      }
      var pattern = new RegExp("^[\\+\\-]?" + digits + "+$", "i");

      return function (s) {
        /* jshint bitwise: false */
        if (pattern.test(s)) {
          var i = parseInt(s, radix);
          return (i | 0) === i ? just(i) : nothing;
        } else {
          return nothing;
        }
      };
    };
  };
};

exports.toStringAs = function (radix) {
  return function (i) {
    return i.toString(radix);
  };
};

},{}],109:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Data_Boolean = require("../Data.Boolean");
var Data_BooleanAlgebra = require("../Data.BooleanAlgebra");
var Data_Bounded = require("../Data.Bounded");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Int_Bits = require("../Data.Int.Bits");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var $$Math = require("../Math");
var Partial_Unsafe = require("../Partial.Unsafe");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Radix = function (x) {
    return x;
};
var radix = function (n) {
    if (n >= 2 && n <= 36) {
        return new Data_Maybe.Just(n);
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Data.Int line 124, column 1 - line 125, column 38: " + [ n.constructor.name ]);
};
var odd = function (x) {
    return Data_Int_Bits.and(x)(1) !== 0;
};
var octal = 8;
var hexadecimal = 16;
var fromStringAs = $foreign.fromStringAsImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var fromString = fromStringAs(10);
var fromNumber = $foreign.fromNumberImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var unsafeClamp = function (x) {
    if (x >= $foreign.toNumber(Data_Bounded.top(Data_Bounded.boundedInt))) {
        return Data_Bounded.top(Data_Bounded.boundedInt);
    };
    if (x <= $foreign.toNumber(Data_Bounded.bottom(Data_Bounded.boundedInt))) {
        return Data_Bounded.bottom(Data_Bounded.boundedInt);
    };
    if (Data_Boolean.otherwise) {
        return Partial_Unsafe.unsafePartial(function (dictPartial) {
            return Data_Maybe.fromJust(dictPartial)(fromNumber(x));
        });
    };
    throw new Error("Failed pattern match at Data.Int line 65, column 1 - line 68, column 56: " + [ x.constructor.name ]);
};
var round = function ($3) {
    return unsafeClamp($$Math.round($3));
};
var floor = function ($4) {
    return unsafeClamp($$Math.floor($4));
};
var even = function (x) {
    return Data_Int_Bits.and(x)(1) === 0;
};
var decimal = 10;
var ceil = function ($5) {
    return unsafeClamp($$Math.ceil($5));
};
var binary = 2;
var base36 = 36;
module.exports = {
    base36: base36, 
    binary: binary, 
    ceil: ceil, 
    decimal: decimal, 
    even: even, 
    floor: floor, 
    fromNumber: fromNumber, 
    fromString: fromString, 
    fromStringAs: fromStringAs, 
    hexadecimal: hexadecimal, 
    octal: octal, 
    odd: odd, 
    radix: radix, 
    round: round, 
    toNumber: $foreign.toNumber, 
    toStringAs: $foreign.toStringAs
};

},{"../Control.Semigroupoid":53,"../Data.Boolean":71,"../Data.BooleanAlgebra":72,"../Data.Bounded":74,"../Data.Eq":79,"../Data.Function":97,"../Data.HeytingAlgebra":104,"../Data.Int.Bits":107,"../Data.Maybe":113,"../Data.Ord":127,"../Math":157,"../Partial.Unsafe":159,"./foreign":108}],110:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Lazy = require("../Control.Lazy");
var Control_MonadPlus = require("../Control.MonadPlus");
var Control_MonadZero = require("../Control.MonadZero");
var Control_Plus = require("../Control.Plus");
var Data_Foldable = require("../Data.Foldable");
var Data_Generic = require("../Data.Generic");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Data_Unfoldable = require("../Data.Unfoldable");
var Control_Apply = require("../Control.Apply");
var Data_Unit = require("../Data.Unit");
var Data_Show = require("../Data.Show");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Functor = require("../Data.Functor");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad = require("../Control.Monad");
var Data_Ring = require("../Data.Ring");
var Data_Boolean = require("../Data.Boolean");
var Data_Semiring = require("../Data.Semiring");
var Data_BooleanAlgebra = require("../Data.BooleanAlgebra");
var Control_Category = require("../Control.Category");
var Nil = (function () {
    function Nil() {

    };
    Nil.value = new Nil();
    return Nil;
})();
var Cons = (function () {
    function Cons(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Cons.create = function (value0) {
        return function (value1) {
            return new Cons(value0, value1);
        };
    };
    return Cons;
})();
var updateAt = function (v) {
    return function (v1) {
        return function (v2) {
            if (v === 0 && v2 instanceof Cons) {
                return new Data_Maybe.Just(new Cons(v1, v2.value1));
            };
            if (v2 instanceof Cons) {
                return Data_Functor.map(Data_Maybe.functorMaybe)(Cons.create(v2.value0))(updateAt(v - 1)(v1)(v2.value1));
            };
            return Data_Maybe.Nothing.value;
        };
    };
};
var uncons = function (v) {
    if (v instanceof Nil) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Cons) {
        return new Data_Maybe.Just({
            head: v.value0, 
            tail: v.value1
        });
    };
    throw new Error("Failed pattern match at Data.List line 251, column 1 - line 251, column 21: " + [ v.constructor.name ]);
};
var toUnfoldable = function (dictUnfoldable) {
    return Data_Unfoldable.unfoldr(dictUnfoldable)(function (xs) {
        return Data_Functor.map(Data_Maybe.functorMaybe)(function (rec) {
            return new Data_Tuple.Tuple(rec.head, rec.tail);
        })(uncons(xs));
    });
};
var tail = function (v) {
    if (v instanceof Nil) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Cons) {
        return new Data_Maybe.Just(v.value1);
    };
    throw new Error("Failed pattern match at Data.List line 232, column 1 - line 232, column 19: " + [ v.constructor.name ]);
};
var span = function (v) {
    return function (v1) {
        if (v1 instanceof Cons && v(v1.value0)) {
            var $138 = span(v)(v1.value1);
            return {
                init: new Cons(v1.value0, $138.init), 
                rest: $138.rest
            };
        };
        return {
            init: Nil.value, 
            rest: v1
        };
    };
};
var singleton = function (a) {
    return new Cons(a, Nil.value);
};
var sortBy = function (cmp) {
    var merge = function (v) {
        return function (v1) {
            if (v instanceof Cons && v1 instanceof Cons) {
                if (Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(v.value0)(v1.value0))(Data_Ordering.GT.value)) {
                    return new Cons(v1.value0, merge(v)(v1.value1));
                };
                if (Data_Boolean.otherwise) {
                    return new Cons(v.value0, merge(v.value1)(v1));
                };
            };
            if (v instanceof Nil) {
                return v1;
            };
            if (v1 instanceof Nil) {
                return v;
            };
            throw new Error("Failed pattern match at Data.List line 456, column 3 - line 458, column 40: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
    var mergePairs = function (v) {
        if (v instanceof Cons && v.value1 instanceof Cons) {
            return new Cons(merge(v.value0)(v.value1.value0), mergePairs(v.value1.value1));
        };
        return v;
    };
    var mergeAll = function (__copy_v) {
        var v = __copy_v;
        tco: while (true) {
            if (v instanceof Cons && v.value1 instanceof Nil) {
                return v.value0;
            };
            var __tco_v = mergePairs(v);
            v = __tco_v;
            continue tco;
        };
    };
    var sequences = function (v) {
        if (v instanceof Cons && v.value1 instanceof Cons) {
            if (Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(v.value0)(v.value1.value0))(Data_Ordering.GT.value)) {
                return descending(v.value1.value0)(singleton(v.value0))(v.value1.value1);
            };
            if (Data_Boolean.otherwise) {
                return ascending(v.value1.value0)(Cons.create(v.value0))(v.value1.value1);
            };
        };
        return singleton(v);
    };
    var descending = function (__copy_a) {
        return function (__copy_as) {
            return function (__copy_v) {
                var a = __copy_a;
                var as = __copy_as;
                var v = __copy_v;
                tco: while (true) {
                    if (v instanceof Cons && Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(a)(v.value0))(Data_Ordering.GT.value)) {
                        var __tco_a = v.value0;
                        var __tco_as = new Cons(a, as);
                        var __tco_v = v.value1;
                        a = __tco_a;
                        as = __tco_as;
                        v = __tco_v;
                        continue tco;
                    };
                    return new Cons(new Cons(a, as), sequences(v));
                };
            };
        };
    };
    var ascending = function (a) {
        return function (as) {
            return function (v) {
                if (v instanceof Cons && Data_Eq.notEq(Data_Ordering.eqOrdering)(cmp(a)(v.value0))(Data_Ordering.GT.value)) {
                    return ascending(v.value0)(function (ys) {
                        return as(new Cons(a, ys));
                    })(v.value1);
                };
                return new Cons(Data_Function.apply(as)(singleton(a)), sequences(v));
            };
        };
    };
    return function ($385) {
        return mergeAll(sequences($385));
    };
};
var sort = function (dictOrd) {
    return function (xs) {
        return sortBy(Data_Ord.compare(dictOrd))(xs);
    };
};
var showList = function (dictShow) {
    return new Data_Show.Show(function (v) {
        if (v instanceof Nil) {
            return "Nil";
        };
        if (v instanceof Cons) {
            return "(Cons " + (Data_Show.show(dictShow)(v.value0) + (" " + (Data_Show.show(showList(dictShow))(v.value1) + ")")));
        };
        throw new Error("Failed pattern match at Data.List line 696, column 3 - line 697, column 3: " + [ v.constructor.name ]);
    });
};
var semigroupList = new Data_Semigroup.Semigroup(function (v) {
    return function (ys) {
        if (v instanceof Nil) {
            return ys;
        };
        if (v instanceof Cons) {
            return new Cons(v.value0, Data_Semigroup.append(semigroupList)(v.value1)(ys));
        };
        throw new Error("Failed pattern match at Data.List line 719, column 3 - line 719, column 21: " + [ v.constructor.name, ys.constructor.name ]);
    };
});
var reverse = (function () {
    var go = function (__copy_acc) {
        return function (__copy_v) {
            var acc = __copy_acc;
            var v = __copy_v;
            tco: while (true) {
                if (v instanceof Nil) {
                    return acc;
                };
                if (v instanceof Cons) {
                    var __tco_acc = new Cons(v.value0, acc);
                    var __tco_v = v.value1;
                    acc = __tco_acc;
                    v = __tco_v;
                    continue tco;
                };
                throw new Error("Failed pattern match at Data.List line 346, column 1 - line 349, column 42: " + [ acc.constructor.name, v.constructor.name ]);
            };
        };
    };
    return go(Nil.value);
})();
var snoc = function (xs) {
    return function (x) {
        return reverse(new Cons(x, reverse(xs)));
    };
};
var take = (function () {
    var go = function (__copy_acc) {
        return function (__copy_v) {
            return function (__copy_v1) {
                var acc = __copy_acc;
                var v = __copy_v;
                var v1 = __copy_v1;
                tco: while (true) {
                    if (v === 0) {
                        return reverse(acc);
                    };
                    if (v1 instanceof Nil) {
                        return reverse(acc);
                    };
                    if (v1 instanceof Cons) {
                        var __tco_acc = new Cons(v1.value0, acc);
                        var __tco_v = v - 1;
                        var __tco_v1 = v1.value1;
                        acc = __tco_acc;
                        v = __tco_v;
                        v1 = __tco_v1;
                        continue tco;
                    };
                    throw new Error("Failed pattern match at Data.List line 474, column 1 - line 478, column 52: " + [ acc.constructor.name, v.constructor.name, v1.constructor.name ]);
                };
            };
        };
    };
    return go(Nil.value);
})();
var takeWhile = function (p) {
    var go = function (__copy_acc) {
        return function (__copy_v) {
            var acc = __copy_acc;
            var v = __copy_v;
            tco: while (true) {
                if (v instanceof Cons && p(v.value0)) {
                    var __tco_acc = new Cons(v.value0, acc);
                    var __tco_v = v.value1;
                    acc = __tco_acc;
                    v = __tco_v;
                    continue tco;
                };
                return reverse(acc);
            };
        };
    };
    return go(Nil.value);
};
var unfoldableList = new Data_Unfoldable.Unfoldable(function (f) {
    return function (b) {
        var go = function (__copy_source) {
            return function (__copy_memo) {
                var source = __copy_source;
                var memo = __copy_memo;
                tco: while (true) {
                    var $192 = f(source);
                    if ($192 instanceof Data_Maybe.Nothing) {
                        return reverse(memo);
                    };
                    if ($192 instanceof Data_Maybe.Just) {
                        var __tco_memo = new Cons($192.value0.value0, memo);
                        source = $192.value0.value1;
                        memo = __tco_memo;
                        continue tco;
                    };
                    throw new Error("Failed pattern match at Data.List line 743, column 24 - line 745, column 57: " + [ $192.constructor.name ]);
                };
            };
        };
        return go(b)(Nil.value);
    };
});
var zipWith = function (f) {
    return function (xs) {
        return function (ys) {
            var go = function (v) {
                return function (v1) {
                    return function (acc) {
                        if (v instanceof Nil) {
                            return acc;
                        };
                        if (v1 instanceof Nil) {
                            return acc;
                        };
                        if (v instanceof Cons && v1 instanceof Cons) {
                            return Data_Function.apply(go(v.value1)(v1.value1))(new Cons(f(v.value0)(v1.value0), acc));
                        };
                        throw new Error("Failed pattern match at Data.List line 638, column 1 - line 642, column 63: " + [ v.constructor.name, v1.constructor.name, acc.constructor.name ]);
                    };
                };
            };
            return Data_Function.apply(reverse)(go(xs)(ys)(Nil.value));
        };
    };
};
var zip = zipWith(Data_Tuple.Tuple.create);
var range = function (start) {
    return function (end) {
        if (start === end) {
            return singleton(start);
        };
        if (Data_Boolean.otherwise) {
            var go = function (__copy_s) {
                return function (__copy_e) {
                    return function (__copy_step) {
                        return function (__copy_rest) {
                            var s = __copy_s;
                            var e = __copy_e;
                            var step = __copy_step;
                            var rest = __copy_rest;
                            tco: while (true) {
                                if (s === e) {
                                    return new Cons(s, rest);
                                };
                                if (Data_Boolean.otherwise) {
                                    var __tco_s = s + step | 0;
                                    var __tco_e = e;
                                    var __tco_step = step;
                                    var __tco_rest = new Cons(s, rest);
                                    s = __tco_s;
                                    e = __tco_e;
                                    step = __tco_step;
                                    rest = __tco_rest;
                                    continue tco;
                                };
                                throw new Error("Failed pattern match at Data.List line 138, column 1 - line 142, column 68: " + [ s.constructor.name, e.constructor.name, step.constructor.name, rest.constructor.name ]);
                            };
                        };
                    };
                };
            };
            return go(end)(start)((function () {
                var $209 = start > end;
                if ($209) {
                    return 1;
                };
                if (!$209) {
                    return -1;
                };
                throw new Error("Failed pattern match at Data.List line 139, column 45 - line 139, column 74: " + [ $209.constructor.name ]);
            })())(Nil.value);
        };
        throw new Error("Failed pattern match at Data.List line 138, column 1 - line 142, column 68: " + [ start.constructor.name, end.constructor.name ]);
    };
};
var $$null = function (v) {
    if (v instanceof Nil) {
        return true;
    };
    return false;
};
var monoidList = new Data_Monoid.Monoid(function () {
    return semigroupList;
}, Nil.value);
var mapWithIndex = function (f) {
    return function (lst) {
        var go = function (v) {
            return function (v1) {
                return function (acc) {
                    if (v1 instanceof Nil) {
                        return acc;
                    };
                    if (v1 instanceof Cons) {
                        return Data_Function.apply(go(v + 1 | 0)(v1.value1))(new Cons(f(v1.value0)(v), acc));
                    };
                    throw new Error("Failed pattern match at Data.List line 412, column 1 - line 415, column 56: " + [ v.constructor.name, v1.constructor.name, acc.constructor.name ]);
                };
            };
        };
        return Data_Function.apply(reverse)(go(0)(lst)(Nil.value));
    };
};
var mapMaybe = function (f) {
    var go = function (__copy_acc) {
        return function (__copy_v) {
            var acc = __copy_acc;
            var v = __copy_v;
            tco: while (true) {
                if (v instanceof Nil) {
                    return reverse(acc);
                };
                if (v instanceof Cons) {
                    var $218 = f(v.value0);
                    if ($218 instanceof Data_Maybe.Nothing) {
                        var __tco_acc = acc;
                        var __tco_v = v.value1;
                        acc = __tco_acc;
                        v = __tco_v;
                        continue tco;
                    };
                    if ($218 instanceof Data_Maybe.Just) {
                        var __tco_acc = new Cons($218.value0, acc);
                        var __tco_v = v.value1;
                        acc = __tco_acc;
                        v = __tco_v;
                        continue tco;
                    };
                    throw new Error("Failed pattern match at Data.List line 400, column 5 - line 402, column 35: " + [ $218.constructor.name ]);
                };
                throw new Error("Failed pattern match at Data.List line 396, column 1 - line 402, column 35: " + [ acc.constructor.name, v.constructor.name ]);
            };
        };
    };
    return go(Nil.value);
};
var some = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Control_Apply.apply((dictAlternative["__superclass_Control.Applicative.Applicative_0"]())["__superclass_Control.Apply.Apply_0"]())(Data_Functor.map(((dictAlternative["__superclass_Control.Plus.Plus_1"]())["__superclass_Control.Alt.Alt_0"]())["__superclass_Data.Functor.Functor_0"]())(Cons.create)(v))(Control_Lazy.defer(dictLazy)(function (v1) {
                return many(dictAlternative)(dictLazy)(v);
            }));
        };
    };
};
var many = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Control_Alt.alt((dictAlternative["__superclass_Control.Plus.Plus_1"]())["__superclass_Control.Alt.Alt_0"]())(some(dictAlternative)(dictLazy)(v))(Control_Applicative.pure(dictAlternative["__superclass_Control.Applicative.Applicative_0"]())(Nil.value));
        };
    };
};
var last = function (__copy_v) {
    var v = __copy_v;
    tco: while (true) {
        if (v instanceof Cons && v.value1 instanceof Nil) {
            return new Data_Maybe.Just(v.value0);
        };
        if (v instanceof Cons) {
            var __tco_v = v.value1;
            v = __tco_v;
            continue tco;
        };
        return Data_Maybe.Nothing.value;
    };
};
var insertBy = function (v) {
    return function (x) {
        return function (v1) {
            if (v1 instanceof Nil) {
                return new Cons(x, Nil.value);
            };
            if (v1 instanceof Cons) {
                var $231 = v(x)(v1.value0);
                if ($231 instanceof Data_Ordering.GT) {
                    return new Cons(v1.value0, insertBy(v)(x)(v1.value1));
                };
                return new Cons(x, v1);
            };
            throw new Error("Failed pattern match at Data.List line 203, column 1 - line 203, column 30: " + [ v.constructor.name, x.constructor.name, v1.constructor.name ]);
        };
    };
};
var insertAt = function (v) {
    return function (v1) {
        return function (v2) {
            if (v === 0) {
                return new Data_Maybe.Just(new Cons(v1, v2));
            };
            if (v2 instanceof Cons) {
                return Data_Functor.map(Data_Maybe.functorMaybe)(Cons.create(v2.value0))(insertAt(v - 1)(v1)(v2.value1));
            };
            return Data_Maybe.Nothing.value;
        };
    };
};
var insert = function (dictOrd) {
    return insertBy(Data_Ord.compare(dictOrd));
};
var init = function (v) {
    if (v instanceof Nil) {
        return Data_Maybe.Nothing.value;
    };
    var go = function (v1) {
        return function (acc) {
            if (v1 instanceof Cons && v1.value1 instanceof Nil) {
                return acc;
            };
            if (v1 instanceof Cons) {
                return Data_Function.apply(go(v1.value1))(new Cons(v1.value0, acc));
            };
            return acc;
        };
    };
    return Data_Function.apply(Data_Maybe.Just.create)(Data_Function.apply(reverse)(go(v)(Nil.value)));
};
var index = function (__copy_v) {
    return function (__copy_v1) {
        var v = __copy_v;
        var v1 = __copy_v1;
        tco: while (true) {
            if (v instanceof Nil) {
                return Data_Maybe.Nothing.value;
            };
            if (v instanceof Cons && v1 === 0) {
                return new Data_Maybe.Just(v.value0);
            };
            if (v instanceof Cons) {
                var __tco_v = v.value1;
                var __tco_v1 = v1 - 1;
                v = __tco_v;
                v1 = __tco_v1;
                continue tco;
            };
            throw new Error("Failed pattern match at Data.List line 262, column 1 - line 262, column 22: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
var head = function (v) {
    if (v instanceof Nil) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Cons) {
        return new Data_Maybe.Just(v.value0);
    };
    throw new Error("Failed pattern match at Data.List line 217, column 1 - line 217, column 19: " + [ v.constructor.name ]);
};
var transpose = function (v) {
    if (v instanceof Nil) {
        return Nil.value;
    };
    if (v instanceof Cons && v.value0 instanceof Nil) {
        return transpose(v.value1);
    };
    if (v instanceof Cons && v.value0 instanceof Cons) {
        return new Cons(new Cons(v.value0.value0, mapMaybe(head)(v.value1)), transpose(new Cons(v.value0.value1, mapMaybe(tail)(v.value1))));
    };
    throw new Error("Failed pattern match at Data.List line 675, column 1 - line 675, column 20: " + [ v.constructor.name ]);
};
var groupBy = function (v) {
    return function (v1) {
        if (v1 instanceof Nil) {
            return Nil.value;
        };
        if (v1 instanceof Cons) {
            var $264 = span(v(v1.value0))(v1.value1);
            return new Cons(new Cons(v1.value0, $264.init), groupBy(v)($264.rest));
        };
        throw new Error("Failed pattern match at Data.List line 548, column 1 - line 548, column 20: " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var group = function (dictEq) {
    return groupBy(Data_Eq.eq(dictEq));
};
var group$prime = function (dictOrd) {
    return function ($386) {
        return group(dictOrd["__superclass_Data.Eq.Eq_0"]())(sort(dictOrd)($386));
    };
};
var genericList = function (dictGeneric) {
    return new Data_Generic.Generic(function (v) {
        if (v instanceof Data_Generic.SProd && (v.value0 === "Data.List.Nil" && v.value1.length === 0)) {
            return new Data_Maybe.Just(Nil.value);
        };
        if (v instanceof Data_Generic.SProd && (v.value0 === "Data.List.Cons" && v.value1.length === 2)) {
            return Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Cons.create))(Data_Generic.fromSpine(dictGeneric)(v.value1[0](Data_Unit.unit))))(Data_Generic.fromSpine(genericList(dictGeneric))(v.value1[1](Data_Unit.unit)));
        };
        return Data_Maybe.Nothing.value;
    }, function ($dollarq) {
        return new Data_Generic.SigProd("Data.List.List", [ {
            sigConstructor: "Data.List.Nil", 
            sigValues: [  ]
        }, {
            sigConstructor: "Data.List.Cons", 
            sigValues: [ function ($dollarq1) {
                return Data_Generic.toSignature(dictGeneric)(Data_Generic.anyProxy);
            }, function ($dollarq1) {
                return Data_Generic.toSignature(genericList(dictGeneric))(Data_Generic.anyProxy);
            } ]
        } ]);
    }, function (v) {
        if (v instanceof Nil) {
            return new Data_Generic.SProd("Data.List.Nil", [  ]);
        };
        if (v instanceof Cons) {
            return new Data_Generic.SProd("Data.List.Cons", [ function ($dollarq) {
                return Data_Generic.toSpine(dictGeneric)(v.value0);
            }, function ($dollarq) {
                return Data_Generic.toSpine(genericList(dictGeneric))(v.value1);
            } ]);
        };
        throw new Error("Failed pattern match: " + [ v.constructor.name ]);
    });
};
var functorList = new Data_Functor.Functor(function (f) {
    return function (lst) {
        var go = function (v) {
            return function (acc) {
                if (v instanceof Nil) {
                    return acc;
                };
                if (v instanceof Cons) {
                    return Data_Function.apply(go(v.value1))(new Cons(f(v.value0), acc));
                };
                throw new Error("Failed pattern match at Data.List line 726, column 3 - line 729, column 48: " + [ v.constructor.name, acc.constructor.name ]);
            };
        };
        return Data_Function.apply(reverse)(go(lst)(Nil.value));
    };
});
var fromFoldable = function (dictFoldable) {
    return Data_Foldable.foldr(dictFoldable)(Cons.create)(Nil.value);
};
var foldableList = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return Data_Foldable.foldl(foldableList)(function (acc) {
            return function ($387) {
                return Data_Semigroup.append(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]())(acc)(f($387));
            };
        })(Data_Monoid.mempty(dictMonoid));
    };
}, (function () {
    var go = function (__copy_v) {
        return function (__copy_b) {
            return function (__copy_v1) {
                var v = __copy_v;
                var b = __copy_b;
                var v1 = __copy_v1;
                tco: while (true) {
                    if (v1 instanceof Nil) {
                        return b;
                    };
                    if (v1 instanceof Cons) {
                        var __tco_v = v;
                        var __tco_b = v(b)(v1.value0);
                        var __tco_v1 = v1.value1;
                        v = __tco_v;
                        b = __tco_b;
                        v1 = __tco_v1;
                        continue tco;
                    };
                    throw new Error("Failed pattern match at Data.List line 734, column 3 - line 737, column 49: " + [ v.constructor.name, b.constructor.name, v1.constructor.name ]);
                };
            };
        };
    };
    return go;
})(), function (v) {
    return function (b) {
        return function (v1) {
            if (v1 instanceof Nil) {
                return b;
            };
            if (v1 instanceof Cons) {
                return v(v1.value0)(Data_Foldable.foldr(foldableList)(v)(b)(v1.value1));
            };
            throw new Error("Failed pattern match at Data.List line 732, column 3 - line 732, column 20: " + [ v.constructor.name, b.constructor.name, v1.constructor.name ]);
        };
    };
});
var length = Data_Foldable.foldl(foldableList)(function (acc) {
    return function (v) {
        return acc + 1 | 0;
    };
})(0);
var traversableList = new Data_Traversable.Traversable(function () {
    return foldableList;
}, function () {
    return functorList;
}, function (dictApplicative) {
    return function (v) {
        if (v instanceof Nil) {
            return Control_Applicative.pure(dictApplicative)(Nil.value);
        };
        if (v instanceof Cons) {
            return Control_Apply.apply(dictApplicative["__superclass_Control.Apply.Apply_0"]())(Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Cons.create)(v.value0))(Data_Traversable.sequence(traversableList)(dictApplicative)(v.value1));
        };
        throw new Error("Failed pattern match at Data.List line 750, column 3 - line 750, column 26: " + [ v.constructor.name ]);
    };
}, function (dictApplicative) {
    return function (v) {
        return function (v1) {
            if (v1 instanceof Nil) {
                return Control_Applicative.pure(dictApplicative)(Nil.value);
            };
            if (v1 instanceof Cons) {
                return Control_Apply.apply(dictApplicative["__superclass_Control.Apply.Apply_0"]())(Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Cons.create)(v(v1.value0)))(Data_Traversable.traverse(traversableList)(dictApplicative)(v)(v1.value1));
            };
            throw new Error("Failed pattern match at Data.List line 748, column 3 - line 748, column 28: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
});
var zipWithA = function (dictApplicative) {
    return function (f) {
        return function (xs) {
            return function (ys) {
                return Data_Traversable.sequence(traversableList)(dictApplicative)(zipWith(f)(xs)(ys));
            };
        };
    };
};
var unzip = Data_Foldable.foldr(foldableList)(function (v) {
    return function (v1) {
        return new Data_Tuple.Tuple(new Cons(v.value0, v1.value0), new Cons(v.value1, v1.value1));
    };
})(new Data_Tuple.Tuple(Nil.value, Nil.value));
var foldM = function (dictMonad) {
    return function (v) {
        return function (a) {
            return function (v1) {
                if (v1 instanceof Nil) {
                    return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())(a);
                };
                if (v1 instanceof Cons) {
                    return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(v(a)(v1.value0))(function (a$prime) {
                        return foldM(dictMonad)(v)(a$prime)(v1.value1);
                    });
                };
                throw new Error("Failed pattern match at Data.List line 686, column 1 - line 686, column 23: " + [ v.constructor.name, a.constructor.name, v1.constructor.name ]);
            };
        };
    };
};
var findIndex = function (fn) {
    var go = function (__copy_v) {
        return function (__copy_v1) {
            var v = __copy_v;
            var v1 = __copy_v1;
            tco: while (true) {
                if (v1 instanceof Cons) {
                    if (fn(v1.value0)) {
                        return new Data_Maybe.Just(v);
                    };
                    if (Data_Boolean.otherwise) {
                        var __tco_v = v + 1 | 0;
                        var __tco_v1 = v1.value1;
                        v = __tco_v;
                        v1 = __tco_v1;
                        continue tco;
                    };
                };
                if (v1 instanceof Nil) {
                    return Data_Maybe.Nothing.value;
                };
                throw new Error("Failed pattern match at Data.List line 282, column 3 - line 283, column 47: " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
    };
    return go(0);
};
var findLastIndex = function (fn) {
    return function (xs) {
        return Data_Functor.map(Data_Maybe.functorMaybe)(function (v) {
            return length(xs) - 1 - v;
        })(findIndex(fn)(reverse(xs)));
    };
};
var filterM = function (dictMonad) {
    return function (v) {
        return function (v1) {
            if (v1 instanceof Nil) {
                return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())(Nil.value);
            };
            if (v1 instanceof Cons) {
                return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(v(v1.value0))(function (v2) {
                    return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(filterM(dictMonad)(v)(v1.value1))(function (v3) {
                        return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())((function () {
                            if (v2) {
                                return new Cons(v1.value0, v3);
                            };
                            if (!v2) {
                                return v3;
                            };
                            throw new Error("Failed pattern match at Data.List line 389, column 3 - line 389, column 37: " + [ v2.constructor.name ]);
                        })());
                    });
                });
            };
            throw new Error("Failed pattern match at Data.List line 385, column 1 - line 385, column 25: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
var filter = function (p) {
    var go = function (__copy_acc) {
        return function (__copy_v) {
            var acc = __copy_acc;
            var v = __copy_v;
            tco: while (true) {
                if (v instanceof Nil) {
                    return reverse(acc);
                };
                if (v instanceof Cons) {
                    if (p(v.value0)) {
                        var __tco_acc = new Cons(v.value0, acc);
                        var __tco_v = v.value1;
                        acc = __tco_acc;
                        v = __tco_v;
                        continue tco;
                    };
                    if (Data_Boolean.otherwise) {
                        var __tco_acc = acc;
                        var __tco_v = v.value1;
                        acc = __tco_acc;
                        v = __tco_v;
                        continue tco;
                    };
                };
                throw new Error("Failed pattern match at Data.List line 369, column 1 - line 374, column 28: " + [ acc.constructor.name, v.constructor.name ]);
            };
        };
    };
    return go(Nil.value);
};
var intersectBy = function (v) {
    return function (v1) {
        return function (v2) {
            if (v1 instanceof Nil) {
                return Nil.value;
            };
            if (v2 instanceof Nil) {
                return Nil.value;
            };
            return filter(function (x) {
                return Data_Foldable.any(foldableList)(Data_BooleanAlgebra.booleanAlgebraBoolean)(v(x))(v2);
            })(v1);
        };
    };
};
var intersect = function (dictEq) {
    return intersectBy(Data_Eq.eq(dictEq));
};
var nubBy = function (v) {
    return function (v1) {
        if (v1 instanceof Nil) {
            return Nil.value;
        };
        if (v1 instanceof Cons) {
            return new Cons(v1.value0, nubBy(v)(filter(function (y) {
                return !v(v1.value0)(y);
            })(v1.value1)));
        };
        throw new Error("Failed pattern match at Data.List line 567, column 1 - line 567, column 22: " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var nub = function (dictEq) {
    return nubBy(Data_Eq.eq(dictEq));
};
var eqList = function (dictEq) {
    return new Data_Eq.Eq(function (xs) {
        return function (ys) {
            var go = function (v) {
                return function (v1) {
                    return function (v2) {
                        if (!v2) {
                            return false;
                        };
                        if (v instanceof Nil && v1 instanceof Nil) {
                            return v2;
                        };
                        if (v instanceof Cons && v1 instanceof Cons) {
                            return Data_Function.apply(go(v.value1)(v1.value1))(v2 && Data_Eq.eq(dictEq)(v1.value0)(v.value0));
                        };
                        return false;
                    };
                };
            };
            return go(xs)(ys)(true);
        };
    });
};
var ordList = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqList(dictOrd["__superclass_Data.Eq.Eq_0"]());
    }, function (xs) {
        return function (ys) {
            var go = function (__copy_v) {
                return function (__copy_v1) {
                    var v = __copy_v;
                    var v1 = __copy_v1;
                    tco: while (true) {
                        if (v instanceof Nil && v1 instanceof Nil) {
                            return Data_Ordering.EQ.value;
                        };
                        if (v instanceof Nil) {
                            return Data_Ordering.LT.value;
                        };
                        if (v1 instanceof Nil) {
                            return Data_Ordering.GT.value;
                        };
                        if (v instanceof Cons && v1 instanceof Cons) {
                            var $343 = Data_Ord.compare(dictOrd)(v.value0)(v1.value0);
                            if ($343 instanceof Data_Ordering.EQ) {
                                var __tco_v = v.value1;
                                var __tco_v1 = v1.value1;
                                v = __tco_v;
                                v1 = __tco_v1;
                                continue tco;
                            };
                            return $343;
                        };
                        throw new Error("Failed pattern match at Data.List line 708, column 3 - line 716, column 23: " + [ v.constructor.name, v1.constructor.name ]);
                    };
                };
            };
            return go(xs)(ys);
        };
    });
};
var elemLastIndex = function (dictEq) {
    return function (x) {
        return findLastIndex(function (v) {
            return Data_Eq.eq(dictEq)(v)(x);
        });
    };
};
var elemIndex = function (dictEq) {
    return function (x) {
        return findIndex(function (v) {
            return Data_Eq.eq(dictEq)(v)(x);
        });
    };
};
var dropWhile = function (p) {
    var go = function (__copy_v) {
        var v = __copy_v;
        tco: while (true) {
            if (v instanceof Cons && p(v.value0)) {
                var __tco_v = v.value1;
                v = __tco_v;
                continue tco;
            };
            return v;
        };
    };
    return go;
};
var drop = function (__copy_v) {
    return function (__copy_v1) {
        var v = __copy_v;
        var v1 = __copy_v1;
        tco: while (true) {
            if (v === 0) {
                return v1;
            };
            if (v1 instanceof Nil) {
                return Nil.value;
            };
            if (v1 instanceof Cons) {
                var __tco_v = v - 1;
                var __tco_v1 = v1.value1;
                v = __tco_v;
                v1 = __tco_v1;
                continue tco;
            };
            throw new Error("Failed pattern match at Data.List line 493, column 1 - line 493, column 15: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
var slice = function (start) {
    return function (end) {
        return function (xs) {
            return take(end - start)(drop(start)(xs));
        };
    };
};
var deleteBy = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Nil) {
                return Nil.value;
            };
            if (v2 instanceof Cons && v(v1)(v2.value0)) {
                return v2.value1;
            };
            if (v2 instanceof Cons) {
                return new Cons(v2.value0, deleteBy(v)(v1)(v2.value1));
            };
            throw new Error("Failed pattern match at Data.List line 594, column 1 - line 594, column 23: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
};
var unionBy = function (eq) {
    return function (xs) {
        return function (ys) {
            return Data_Semigroup.append(semigroupList)(xs)(Data_Foldable.foldl(foldableList)(Data_Function.flip(deleteBy(eq)))(nubBy(eq)(ys))(xs));
        };
    };
};
var union = function (dictEq) {
    return unionBy(Data_Eq.eq(dictEq));
};
var deleteAt = function (v) {
    return function (v1) {
        if (v === 0 && v1 instanceof Cons) {
            return new Data_Maybe.Just(v1.value1);
        };
        if (v1 instanceof Cons) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(Cons.create(v1.value0))(deleteAt(v - 1)(v1.value1));
        };
        return Data_Maybe.Nothing.value;
    };
};
var $$delete = function (dictEq) {
    return deleteBy(Data_Eq.eq(dictEq));
};
var difference = function (dictEq) {
    return Data_Foldable.foldl(foldableList)(Data_Function.flip($$delete(dictEq)));
};
var concatMap = function (v) {
    return function (v1) {
        if (v1 instanceof Nil) {
            return Nil.value;
        };
        if (v1 instanceof Cons) {
            return Data_Semigroup.append(semigroupList)(v(v1.value0))(concatMap(v)(v1.value1));
        };
        throw new Error("Failed pattern match at Data.List line 362, column 1 - line 362, column 22: " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var catMaybes = mapMaybe(Control_Category.id(Control_Category.categoryFn));
var applyList = new Control_Apply.Apply(function () {
    return functorList;
}, function (v) {
    return function (v1) {
        if (v instanceof Nil) {
            return Nil.value;
        };
        if (v instanceof Cons) {
            return Data_Semigroup.append(semigroupList)(Data_Functor.map(functorList)(v.value0)(v1))(Control_Apply.apply(applyList)(v.value1)(v1));
        };
        throw new Error("Failed pattern match at Data.List line 754, column 3 - line 754, column 20: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var bindList = new Control_Bind.Bind(function () {
    return applyList;
}, Data_Function.flip(concatMap));
var concat = function (v) {
    return Control_Bind.bind(bindList)(v)(Control_Category.id(Control_Category.categoryFn));
};
var applicativeList = new Control_Applicative.Applicative(function () {
    return applyList;
}, function (a) {
    return new Cons(a, Nil.value);
});
var monadList = new Control_Monad.Monad(function () {
    return applicativeList;
}, function () {
    return bindList;
});
var alterAt = function (v) {
    return function (v1) {
        return function (v2) {
            if (v === 0 && v2 instanceof Cons) {
                return Data_Function.apply(Data_Maybe.Just.create)((function () {
                    var $379 = v1(v2.value0);
                    if ($379 instanceof Data_Maybe.Nothing) {
                        return v2.value1;
                    };
                    if ($379 instanceof Data_Maybe.Just) {
                        return new Cons($379.value0, v2.value1);
                    };
                    throw new Error("Failed pattern match at Data.List line 331, column 27 - line 334, column 26: " + [ $379.constructor.name ]);
                })());
            };
            if (v2 instanceof Cons) {
                return Data_Functor.map(Data_Maybe.functorMaybe)(Cons.create(v2.value0))(alterAt(v - 1)(v1)(v2.value1));
            };
            return Data_Maybe.Nothing.value;
        };
    };
};
var modifyAt = function (n) {
    return function (f) {
        return alterAt(n)(function ($388) {
            return Data_Maybe.Just.create(f($388));
        });
    };
};
var altList = new Control_Alt.Alt(function () {
    return functorList;
}, Data_Semigroup.append(semigroupList));
var plusList = new Control_Plus.Plus(function () {
    return altList;
}, Nil.value);
var alternativeList = new Control_Alternative.Alternative(function () {
    return applicativeList;
}, function () {
    return plusList;
});
var monadZeroList = new Control_MonadZero.MonadZero(function () {
    return alternativeList;
}, function () {
    return monadList;
});
var monadPlusList = new Control_MonadPlus.MonadPlus(function () {
    return monadZeroList;
});
module.exports = {
    Nil: Nil, 
    Cons: Cons, 
    alterAt: alterAt, 
    catMaybes: catMaybes, 
    concat: concat, 
    concatMap: concatMap, 
    "delete": $$delete, 
    deleteAt: deleteAt, 
    deleteBy: deleteBy, 
    difference: difference, 
    drop: drop, 
    dropWhile: dropWhile, 
    elemIndex: elemIndex, 
    elemLastIndex: elemLastIndex, 
    filter: filter, 
    filterM: filterM, 
    findIndex: findIndex, 
    findLastIndex: findLastIndex, 
    foldM: foldM, 
    fromFoldable: fromFoldable, 
    group: group, 
    "group'": group$prime, 
    groupBy: groupBy, 
    head: head, 
    index: index, 
    init: init, 
    insert: insert, 
    insertAt: insertAt, 
    insertBy: insertBy, 
    intersect: intersect, 
    intersectBy: intersectBy, 
    last: last, 
    length: length, 
    many: many, 
    mapMaybe: mapMaybe, 
    mapWithIndex: mapWithIndex, 
    modifyAt: modifyAt, 
    nub: nub, 
    nubBy: nubBy, 
    "null": $$null, 
    range: range, 
    reverse: reverse, 
    singleton: singleton, 
    slice: slice, 
    snoc: snoc, 
    some: some, 
    sort: sort, 
    sortBy: sortBy, 
    span: span, 
    tail: tail, 
    take: take, 
    takeWhile: takeWhile, 
    toUnfoldable: toUnfoldable, 
    transpose: transpose, 
    uncons: uncons, 
    union: union, 
    unionBy: unionBy, 
    unzip: unzip, 
    updateAt: updateAt, 
    zip: zip, 
    zipWith: zipWith, 
    zipWithA: zipWithA, 
    genericList: genericList, 
    showList: showList, 
    eqList: eqList, 
    ordList: ordList, 
    semigroupList: semigroupList, 
    monoidList: monoidList, 
    functorList: functorList, 
    foldableList: foldableList, 
    unfoldableList: unfoldableList, 
    traversableList: traversableList, 
    applyList: applyList, 
    applicativeList: applicativeList, 
    bindList: bindList, 
    monadList: monadList, 
    altList: altList, 
    plusList: plusList, 
    alternativeList: alternativeList, 
    monadZeroList: monadZeroList, 
    monadPlusList: monadPlusList
};

},{"../Control.Alt":5,"../Control.Alternative":6,"../Control.Applicative":7,"../Control.Apply":9,"../Control.Bind":13,"../Control.Category":14,"../Control.Lazy":17,"../Control.Monad":48,"../Control.MonadPlus":49,"../Control.MonadZero":50,"../Control.Plus":52,"../Control.Semigroupoid":53,"../Data.Boolean":71,"../Data.BooleanAlgebra":72,"../Data.Eq":79,"../Data.Foldable":84,"../Data.Function":97,"../Data.Functor":100,"../Data.Generic":102,"../Data.HeytingAlgebra":104,"../Data.Maybe":113,"../Data.Monoid":120,"../Data.Ord":127,"../Data.Ordering":128,"../Data.Ring":130,"../Data.Semigroup":132,"../Data.Semiring":134,"../Data.Show":136,"../Data.Traversable":146,"../Data.Tuple":147,"../Data.Unfoldable":149,"../Data.Unit":151,"../Prelude":162}],111:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Extend = require("../Control.Extend");
var Control_Monad = require("../Control.Monad");
var Data_Bounded = require("../Data.Bounded");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var First = function (x) {
    return x;
};
var showFirst = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "First (" + (Data_Show.show(Data_Maybe.showMaybe(dictShow))(v) + ")");
    });
};
var semigroupFirst = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        if (v instanceof Data_Maybe.Just) {
            return v;
        };
        return v1;
    };
});
var runFirst = function (v) {
    return v;
};
var monoidFirst = new Data_Monoid.Monoid(function () {
    return semigroupFirst;
}, Data_Maybe.Nothing.value);
var functorFirst = new Data_Functor.Functor(function (f) {
    return function (v) {
        return Data_Functor.map(Data_Maybe.functorMaybe)(f)(v);
    };
});
var invariantFirst = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorFirst));
var extendFirst = new Control_Extend.Extend(function () {
    return functorFirst;
}, function (f) {
    return function (v) {
        return Control_Extend.extend(Data_Maybe.extendMaybe)(function ($34) {
            return f(First($34));
        })(v);
    };
});
var eqFirst = function (dictEq) {
    return new Data_Eq.Eq(function (v) {
        return function (v1) {
            return Data_Eq.eq(Data_Maybe.eqMaybe(dictEq))(v)(v1);
        };
    });
};
var ordFirst = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqFirst(dictOrd["__superclass_Data.Eq.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Data_Ord.compare(Data_Maybe.ordMaybe(dictOrd))(v)(v1);
        };
    });
};
var boundedFirst = function (dictBounded) {
    return new Data_Bounded.Bounded(function () {
        return ordFirst(dictBounded["__superclass_Data.Ord.Ord_0"]());
    }, Data_Bounded.bottom(Data_Maybe.boundedMaybe(dictBounded)), Data_Bounded.top(Data_Maybe.boundedMaybe(dictBounded)));
};
var applyFirst = new Control_Apply.Apply(function () {
    return functorFirst;
}, function (v) {
    return function (v1) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(v)(v1);
    };
});
var bindFirst = new Control_Bind.Bind(function () {
    return applyFirst;
}, function (v) {
    return function (f) {
        return Control_Bind.bind(Data_Maybe.bindMaybe)(v)(function ($35) {
            return runFirst(f($35));
        });
    };
});
var applicativeFirst = new Control_Applicative.Applicative(function () {
    return applyFirst;
}, function ($36) {
    return First(Control_Applicative.pure(Data_Maybe.applicativeMaybe)($36));
});
var monadFirst = new Control_Monad.Monad(function () {
    return applicativeFirst;
}, function () {
    return bindFirst;
});
module.exports = {
    First: First, 
    runFirst: runFirst, 
    eqFirst: eqFirst, 
    ordFirst: ordFirst, 
    boundedFirst: boundedFirst, 
    functorFirst: functorFirst, 
    invariantFirst: invariantFirst, 
    applyFirst: applyFirst, 
    applicativeFirst: applicativeFirst, 
    bindFirst: bindFirst, 
    monadFirst: monadFirst, 
    extendFirst: extendFirst, 
    showFirst: showFirst, 
    semigroupFirst: semigroupFirst, 
    monoidFirst: monoidFirst
};

},{"../Control.Applicative":7,"../Control.Apply":9,"../Control.Bind":13,"../Control.Extend":16,"../Control.Monad":48,"../Control.Semigroupoid":53,"../Data.Bounded":74,"../Data.Eq":79,"../Data.Function":97,"../Data.Functor":100,"../Data.Functor.Invariant":98,"../Data.Maybe":113,"../Data.Monoid":120,"../Data.Ord":127,"../Data.Semigroup":132,"../Data.Show":136}],112:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Extend = require("../Control.Extend");
var Control_Monad = require("../Control.Monad");
var Data_Bounded = require("../Data.Bounded");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Last = function (x) {
    return x;
};
var showLast = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Last " + (Data_Show.show(Data_Maybe.showMaybe(dictShow))(v) + ")");
    });
};
var semigroupLast = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        if (v1 instanceof Data_Maybe.Just) {
            return v1;
        };
        if (v1 instanceof Data_Maybe.Nothing) {
            return v;
        };
        throw new Error("Failed pattern match at Data.Maybe.Last line 67, column 3 - line 67, column 39: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var runLast = function (v) {
    return v;
};
var monoidLast = new Data_Monoid.Monoid(function () {
    return semigroupLast;
}, Data_Maybe.Nothing.value);
var functorLast = new Data_Functor.Functor(function (f) {
    return function (v) {
        return Data_Functor.map(Data_Maybe.functorMaybe)(f)(v);
    };
});
var invariantLast = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorLast));
var extendLast = new Control_Extend.Extend(function () {
    return functorLast;
}, function (f) {
    return function (v) {
        return Control_Extend.extend(Data_Maybe.extendMaybe)(function ($34) {
            return f(Last($34));
        })(v);
    };
});
var eqLast = function (dictEq) {
    return new Data_Eq.Eq(function (v) {
        return function (v1) {
            return Data_Eq.eq(Data_Maybe.eqMaybe(dictEq))(v)(v1);
        };
    });
};
var ordLast = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqLast(dictOrd["__superclass_Data.Eq.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Data_Ord.compare(Data_Maybe.ordMaybe(dictOrd))(v)(v1);
        };
    });
};
var boundedLast = function (dictBounded) {
    return new Data_Bounded.Bounded(function () {
        return ordLast(dictBounded["__superclass_Data.Ord.Ord_0"]());
    }, Data_Bounded.bottom(Data_Maybe.boundedMaybe(dictBounded)), Data_Bounded.top(Data_Maybe.boundedMaybe(dictBounded)));
};
var applyLast = new Control_Apply.Apply(function () {
    return functorLast;
}, function (v) {
    return function (v1) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(v)(v1);
    };
});
var bindLast = new Control_Bind.Bind(function () {
    return applyLast;
}, function (v) {
    return function (f) {
        return Control_Bind.bind(Data_Maybe.bindMaybe)(v)(function ($35) {
            return runLast(f($35));
        });
    };
});
var applicativeLast = new Control_Applicative.Applicative(function () {
    return applyLast;
}, function ($36) {
    return Last(Control_Applicative.pure(Data_Maybe.applicativeMaybe)($36));
});
var monadLast = new Control_Monad.Monad(function () {
    return applicativeLast;
}, function () {
    return bindLast;
});
module.exports = {
    Last: Last, 
    runLast: runLast, 
    eqLast: eqLast, 
    ordLast: ordLast, 
    boundedLast: boundedLast, 
    functorLast: functorLast, 
    invariantLast: invariantLast, 
    applyLast: applyLast, 
    applicativeLast: applicativeLast, 
    bindLast: bindLast, 
    monadLast: monadLast, 
    extendLast: extendLast, 
    showLast: showLast, 
    semigroupLast: semigroupLast, 
    monoidLast: monoidLast
};

},{"../Control.Applicative":7,"../Control.Apply":9,"../Control.Bind":13,"../Control.Extend":16,"../Control.Monad":48,"../Control.Semigroupoid":53,"../Data.Bounded":74,"../Data.Eq":79,"../Data.Function":97,"../Data.Functor":100,"../Data.Functor.Invariant":98,"../Data.Maybe":113,"../Data.Monoid":120,"../Data.Ord":127,"../Data.Semigroup":132,"../Data.Show":136}],113:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Extend = require("../Control.Extend");
var Control_Monad = require("../Control.Monad");
var Control_MonadZero = require("../Control.MonadZero");
var Control_Plus = require("../Control.Plus");
var Data_Bounded = require("../Data.Bounded");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Monoid = require("../Data.Monoid");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_Unit = require("../Data.Unit");
var Control_Category = require("../Control.Category");
var Just = (function () {
    function Just(value0) {
        this.value0 = value0;
    };
    Just.create = function (value0) {
        return new Just(value0);
    };
    return Just;
})();
var Nothing = (function () {
    function Nothing() {

    };
    Nothing.value = new Nothing();
    return Nothing;
})();
var showMaybe = function (dictShow) {
    return new Data_Show.Show(function (v) {
        if (v instanceof Just) {
            return "(Just " + (Data_Show.show(dictShow)(v.value0) + ")");
        };
        if (v instanceof Nothing) {
            return "Nothing";
        };
        throw new Error("Failed pattern match at Data.Maybe line 220, column 3 - line 221, column 3: " + [ v.constructor.name ]);
    });
};
var semigroupMaybe = function (dictSemigroup) {
    return new Data_Semigroup.Semigroup(function (v) {
        return function (v1) {
            if (v instanceof Nothing) {
                return v1;
            };
            if (v1 instanceof Nothing) {
                return v;
            };
            if (v instanceof Just && v1 instanceof Just) {
                return new Just(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0));
            };
            throw new Error("Failed pattern match at Data.Maybe line 186, column 3 - line 186, column 23: " + [ v.constructor.name, v1.constructor.name ]);
        };
    });
};
var monoidMaybe = function (dictSemigroup) {
    return new Data_Monoid.Monoid(function () {
        return semigroupMaybe(dictSemigroup);
    }, Nothing.value);
};
var maybe$prime = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Nothing) {
                return v(Data_Unit.unit);
            };
            if (v2 instanceof Just) {
                return v1(v2.value0);
            };
            throw new Error("Failed pattern match at Data.Maybe line 245, column 1 - line 245, column 28: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
};
var maybe = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Nothing) {
                return v;
            };
            if (v2 instanceof Just) {
                return v1(v2.value0);
            };
            throw new Error("Failed pattern match at Data.Maybe line 232, column 1 - line 232, column 22: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
};
var isNothing = maybe(true)(Data_Function["const"](false));
var isJust = maybe(false)(Data_Function["const"](true));
var functorMaybe = new Data_Functor.Functor(function (v) {
    return function (v1) {
        if (v1 instanceof Just) {
            return new Just(v(v1.value0));
        };
        return Nothing.value;
    };
});
var invariantMaybe = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorMaybe));
var fromMaybe$prime = function (a) {
    return maybe$prime(a)(Control_Category.id(Control_Category.categoryFn));
};
var fromMaybe = function (a) {
    return maybe(a)(Control_Category.id(Control_Category.categoryFn));
};
var fromJust = function (dictPartial) {
    return function (v) {
        var __unused = function (dictPartial1) {
            return function ($dollar29) {
                return $dollar29;
            };
        };
        return __unused(dictPartial)((function () {
            if (v instanceof Just) {
                return v.value0;
            };
            throw new Error("Failed pattern match at Data.Maybe line 283, column 1 - line 283, column 21: " + [ v.constructor.name ]);
        })());
    };
};
var extendMaybe = new Control_Extend.Extend(function () {
    return functorMaybe;
}, function (v) {
    return function (v1) {
        if (v1 instanceof Nothing) {
            return Nothing.value;
        };
        return new Just(v(v1));
    };
});
var eqMaybe = function (dictEq) {
    return new Data_Eq.Eq(function (v) {
        return function (v1) {
            if (v instanceof Nothing && v1 instanceof Nothing) {
                return true;
            };
            if (v instanceof Just && v1 instanceof Just) {
                return Data_Eq.eq(dictEq)(v.value0)(v1.value0);
            };
            return false;
        };
    });
};
var ordMaybe = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqMaybe(dictOrd["__superclass_Data.Eq.Eq_0"]());
    }, function (v) {
        return function (v1) {
            if (v instanceof Just && v1 instanceof Just) {
                return Data_Ord.compare(dictOrd)(v.value0)(v1.value0);
            };
            if (v instanceof Nothing && v1 instanceof Nothing) {
                return Data_Ordering.EQ.value;
            };
            if (v instanceof Nothing) {
                return Data_Ordering.LT.value;
            };
            if (v1 instanceof Nothing) {
                return Data_Ordering.GT.value;
            };
            throw new Error("Failed pattern match at Data.Maybe line 207, column 3 - line 207, column 42: " + [ v.constructor.name, v1.constructor.name ]);
        };
    });
};
var boundedMaybe = function (dictBounded) {
    return new Data_Bounded.Bounded(function () {
        return ordMaybe(dictBounded["__superclass_Data.Ord.Ord_0"]());
    }, Nothing.value, new Just(Data_Bounded.top(dictBounded)));
};
var applyMaybe = new Control_Apply.Apply(function () {
    return functorMaybe;
}, function (v) {
    return function (v1) {
        if (v instanceof Just) {
            return Data_Functor.map(functorMaybe)(v.value0)(v1);
        };
        if (v instanceof Nothing) {
            return Nothing.value;
        };
        throw new Error("Failed pattern match at Data.Maybe line 78, column 3 - line 78, column 31: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var bindMaybe = new Control_Bind.Bind(function () {
    return applyMaybe;
}, function (v) {
    return function (v1) {
        if (v instanceof Just) {
            return v1(v.value0);
        };
        if (v instanceof Nothing) {
            return Nothing.value;
        };
        throw new Error("Failed pattern match at Data.Maybe line 137, column 3 - line 137, column 24: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var applicativeMaybe = new Control_Applicative.Applicative(function () {
    return applyMaybe;
}, Just.create);
var monadMaybe = new Control_Monad.Monad(function () {
    return applicativeMaybe;
}, function () {
    return bindMaybe;
});
var altMaybe = new Control_Alt.Alt(function () {
    return functorMaybe;
}, function (v) {
    return function (v1) {
        if (v instanceof Nothing) {
            return v1;
        };
        return v;
    };
});
var plusMaybe = new Control_Plus.Plus(function () {
    return altMaybe;
}, Nothing.value);
var alternativeMaybe = new Control_Alternative.Alternative(function () {
    return applicativeMaybe;
}, function () {
    return plusMaybe;
});
var monadZeroMaybe = new Control_MonadZero.MonadZero(function () {
    return alternativeMaybe;
}, function () {
    return monadMaybe;
});
module.exports = {
    Just: Just, 
    Nothing: Nothing, 
    fromJust: fromJust, 
    fromMaybe: fromMaybe, 
    "fromMaybe'": fromMaybe$prime, 
    isJust: isJust, 
    isNothing: isNothing, 
    maybe: maybe, 
    "maybe'": maybe$prime, 
    functorMaybe: functorMaybe, 
    applyMaybe: applyMaybe, 
    applicativeMaybe: applicativeMaybe, 
    altMaybe: altMaybe, 
    plusMaybe: plusMaybe, 
    alternativeMaybe: alternativeMaybe, 
    bindMaybe: bindMaybe, 
    monadMaybe: monadMaybe, 
    monadZeroMaybe: monadZeroMaybe, 
    extendMaybe: extendMaybe, 
    invariantMaybe: invariantMaybe, 
    semigroupMaybe: semigroupMaybe, 
    monoidMaybe: monoidMaybe, 
    eqMaybe: eqMaybe, 
    ordMaybe: ordMaybe, 
    boundedMaybe: boundedMaybe, 
    showMaybe: showMaybe
};

},{"../Control.Alt":5,"../Control.Alternative":6,"../Control.Applicative":7,"../Control.Apply":9,"../Control.Bind":13,"../Control.Category":14,"../Control.Extend":16,"../Control.Monad":48,"../Control.MonadZero":50,"../Control.Plus":52,"../Data.Bounded":74,"../Data.Eq":79,"../Data.Function":97,"../Data.Functor":100,"../Data.Functor.Invariant":98,"../Data.Monoid":120,"../Data.Ord":127,"../Data.Ordering":128,"../Data.Semigroup":132,"../Data.Show":136,"../Data.Unit":151}],114:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Comonad = require("../Control.Comonad");
var Control_Extend = require("../Control.Extend");
var Control_Monad = require("../Control.Monad");
var Data_Bounded = require("../Data.Bounded");
var Data_Eq = require("../Data.Eq");
var Data_Functor = require("../Data.Functor");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Monoid = require("../Data.Monoid");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Additive = function (x) {
    return x;
};
var showAdditive = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Additive " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var semigroupAdditive = function (dictSemiring) {
    return new Data_Semigroup.Semigroup(function (v) {
        return function (v1) {
            return Data_Semiring.add(dictSemiring)(v)(v1);
        };
    });
};
var runAdditive = function (v) {
    return v;
};
var monoidAdditive = function (dictSemiring) {
    return new Data_Monoid.Monoid(function () {
        return semigroupAdditive(dictSemiring);
    }, Data_Semiring.zero(dictSemiring));
};
var invariantAdditive = new Data_Functor_Invariant.Invariant(function (f) {
    return function (v) {
        return function (v1) {
            return f(v1);
        };
    };
});
var functorAdditive = new Data_Functor.Functor(function (f) {
    return function (v) {
        return f(v);
    };
});
var extendAdditive = new Control_Extend.Extend(function () {
    return functorAdditive;
}, function (f) {
    return function (x) {
        return f(x);
    };
});
var eqAdditive = function (dictEq) {
    return new Data_Eq.Eq(function (v) {
        return function (v1) {
            return Data_Eq.eq(dictEq)(v)(v1);
        };
    });
};
var ordAdditive = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqAdditive(dictOrd["__superclass_Data.Eq.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Data_Ord.compare(dictOrd)(v)(v1);
        };
    });
};
var comonadAdditive = new Control_Comonad.Comonad(function () {
    return extendAdditive;
}, runAdditive);
var boundedAdditive = function (dictBounded) {
    return new Data_Bounded.Bounded(function () {
        return ordAdditive(dictBounded["__superclass_Data.Ord.Ord_0"]());
    }, Data_Bounded.bottom(dictBounded), Data_Bounded.top(dictBounded));
};
var applyAdditive = new Control_Apply.Apply(function () {
    return functorAdditive;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindAdditive = new Control_Bind.Bind(function () {
    return applyAdditive;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeAdditive = new Control_Applicative.Applicative(function () {
    return applyAdditive;
}, Additive);
var monadAdditive = new Control_Monad.Monad(function () {
    return applicativeAdditive;
}, function () {
    return bindAdditive;
});
module.exports = {
    Additive: Additive, 
    runAdditive: runAdditive, 
    eqAdditive: eqAdditive, 
    ordAdditive: ordAdditive, 
    boundedAdditive: boundedAdditive, 
    functorAdditive: functorAdditive, 
    invariantAdditive: invariantAdditive, 
    applyAdditive: applyAdditive, 
    applicativeAdditive: applicativeAdditive, 
    bindAdditive: bindAdditive, 
    monadAdditive: monadAdditive, 
    extendAdditive: extendAdditive, 
    comonadAdditive: comonadAdditive, 
    showAdditive: showAdditive, 
    semigroupAdditive: semigroupAdditive, 
    monoidAdditive: monoidAdditive
};

},{"../Control.Applicative":7,"../Control.Apply":9,"../Control.Bind":13,"../Control.Comonad":15,"../Control.Extend":16,"../Control.Monad":48,"../Data.Bounded":74,"../Data.Eq":79,"../Data.Functor":100,"../Data.Functor.Invariant":98,"../Data.Monoid":120,"../Data.Ord":127,"../Data.Semigroup":132,"../Data.Semiring":134,"../Data.Show":136}],115:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Comonad = require("../Control.Comonad");
var Control_Extend = require("../Control.Extend");
var Control_Monad = require("../Control.Monad");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Bounded = require("../Data.Bounded");
var Data_Eq = require("../Data.Eq");
var Data_Functor = require("../Data.Functor");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Monoid = require("../Data.Monoid");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Conj = function (x) {
    return x;
};
var showConj = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Conj " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var semiringConj = function (dictHeytingAlgebra) {
    return new Data_Semiring.Semiring(function (v) {
        return function (v1) {
            return Data_HeytingAlgebra.conj(dictHeytingAlgebra)(v)(v1);
        };
    }, function (v) {
        return function (v1) {
            return Data_HeytingAlgebra.disj(dictHeytingAlgebra)(v)(v1);
        };
    }, Data_HeytingAlgebra.ff(dictHeytingAlgebra), Data_HeytingAlgebra.tt(dictHeytingAlgebra));
};
var semigroupConj = function (dictHeytingAlgebra) {
    return new Data_Semigroup.Semigroup(function (v) {
        return function (v1) {
            return Data_HeytingAlgebra.conj(dictHeytingAlgebra)(v)(v1);
        };
    });
};
var runConj = function (v) {
    return v;
};
var monoidConj = function (dictHeytingAlgebra) {
    return new Data_Monoid.Monoid(function () {
        return semigroupConj(dictHeytingAlgebra);
    }, Data_HeytingAlgebra.tt(dictHeytingAlgebra));
};
var invariantConj = new Data_Functor_Invariant.Invariant(function (f) {
    return function (v) {
        return function (v1) {
            return f(v1);
        };
    };
});
var functorConj = new Data_Functor.Functor(function (f) {
    return function (v) {
        return f(v);
    };
});
var extendConj = new Control_Extend.Extend(function () {
    return functorConj;
}, function (f) {
    return function (x) {
        return f(x);
    };
});
var eqConj = function (dictEq) {
    return new Data_Eq.Eq(function (v) {
        return function (v1) {
            return Data_Eq.eq(dictEq)(v)(v1);
        };
    });
};
var ordConj = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqConj(dictOrd["__superclass_Data.Eq.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Data_Ord.compare(dictOrd)(v)(v1);
        };
    });
};
var comonadConj = new Control_Comonad.Comonad(function () {
    return extendConj;
}, runConj);
var boundedConj = function (dictBounded) {
    return new Data_Bounded.Bounded(function () {
        return ordConj(dictBounded["__superclass_Data.Ord.Ord_0"]());
    }, Data_Bounded.bottom(dictBounded), Data_Bounded.top(dictBounded));
};
var applyConj = new Control_Apply.Apply(function () {
    return functorConj;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindConj = new Control_Bind.Bind(function () {
    return applyConj;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeConj = new Control_Applicative.Applicative(function () {
    return applyConj;
}, Conj);
var monadConj = new Control_Monad.Monad(function () {
    return applicativeConj;
}, function () {
    return bindConj;
});
module.exports = {
    Conj: Conj, 
    runConj: runConj, 
    eqConj: eqConj, 
    ordConj: ordConj, 
    boundedConj: boundedConj, 
    functorConj: functorConj, 
    invariantConj: invariantConj, 
    applyConj: applyConj, 
    applicativeConj: applicativeConj, 
    bindConj: bindConj, 
    monadConj: monadConj, 
    extendConj: extendConj, 
    comonadConj: comonadConj, 
    showConj: showConj, 
    semigroupConj: semigroupConj, 
    monoidConj: monoidConj, 
    semiringConj: semiringConj
};

},{"../Control.Applicative":7,"../Control.Apply":9,"../Control.Bind":13,"../Control.Comonad":15,"../Control.Extend":16,"../Control.Monad":48,"../Data.Bounded":74,"../Data.Eq":79,"../Data.Functor":100,"../Data.Functor.Invariant":98,"../Data.HeytingAlgebra":104,"../Data.Monoid":120,"../Data.Ord":127,"../Data.Semigroup":132,"../Data.Semiring":134,"../Data.Show":136}],116:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Comonad = require("../Control.Comonad");
var Control_Extend = require("../Control.Extend");
var Control_Monad = require("../Control.Monad");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Bounded = require("../Data.Bounded");
var Data_Eq = require("../Data.Eq");
var Data_Functor = require("../Data.Functor");
var Data_Monoid = require("../Data.Monoid");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Disj = function (x) {
    return x;
};
var showDisj = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Disj " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var semiringDisj = function (dictHeytingAlgebra) {
    return new Data_Semiring.Semiring(function (v) {
        return function (v1) {
            return Data_HeytingAlgebra.disj(dictHeytingAlgebra)(v)(v1);
        };
    }, function (v) {
        return function (v1) {
            return Data_HeytingAlgebra.conj(dictHeytingAlgebra)(v)(v1);
        };
    }, Data_HeytingAlgebra.tt(dictHeytingAlgebra), Data_HeytingAlgebra.ff(dictHeytingAlgebra));
};
var semigroupDisj = function (dictHeytingAlgebra) {
    return new Data_Semigroup.Semigroup(function (v) {
        return function (v1) {
            return Data_HeytingAlgebra.disj(dictHeytingAlgebra)(v)(v1);
        };
    });
};
var runDisj = function (v) {
    return v;
};
var monoidDisj = function (dictHeytingAlgebra) {
    return new Data_Monoid.Monoid(function () {
        return semigroupDisj(dictHeytingAlgebra);
    }, Data_HeytingAlgebra.ff(dictHeytingAlgebra));
};
var functorDisj = new Data_Functor.Functor(function (f) {
    return function (v) {
        return f(v);
    };
});
var extendDisj = new Control_Extend.Extend(function () {
    return functorDisj;
}, function (f) {
    return function (x) {
        return f(x);
    };
});
var eqDisj = function (dictEq) {
    return new Data_Eq.Eq(function (v) {
        return function (v1) {
            return Data_Eq.eq(dictEq)(v)(v1);
        };
    });
};
var ordDisj = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqDisj(dictOrd["__superclass_Data.Eq.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Data_Ord.compare(dictOrd)(v)(v1);
        };
    });
};
var comonadDisj = new Control_Comonad.Comonad(function () {
    return extendDisj;
}, runDisj);
var boundedDisj = function (dictBounded) {
    return new Data_Bounded.Bounded(function () {
        return ordDisj(dictBounded["__superclass_Data.Ord.Ord_0"]());
    }, Data_Bounded.bottom(dictBounded), Data_Bounded.top(dictBounded));
};
var applyDisj = new Control_Apply.Apply(function () {
    return functorDisj;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindDisj = new Control_Bind.Bind(function () {
    return applyDisj;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeDisj = new Control_Applicative.Applicative(function () {
    return applyDisj;
}, Disj);
var monadDisj = new Control_Monad.Monad(function () {
    return applicativeDisj;
}, function () {
    return bindDisj;
});
module.exports = {
    Disj: Disj, 
    runDisj: runDisj, 
    eqDisj: eqDisj, 
    ordDisj: ordDisj, 
    boundedDisj: boundedDisj, 
    functorDisj: functorDisj, 
    applyDisj: applyDisj, 
    applicativeDisj: applicativeDisj, 
    bindDisj: bindDisj, 
    monadDisj: monadDisj, 
    extendDisj: extendDisj, 
    comonadDisj: comonadDisj, 
    showDisj: showDisj, 
    semigroupDisj: semigroupDisj, 
    monoidDisj: monoidDisj, 
    semiringDisj: semiringDisj
};

},{"../Control.Applicative":7,"../Control.Apply":9,"../Control.Bind":13,"../Control.Comonad":15,"../Control.Extend":16,"../Control.Monad":48,"../Data.Bounded":74,"../Data.Eq":79,"../Data.Functor":100,"../Data.HeytingAlgebra":104,"../Data.Monoid":120,"../Data.Ord":127,"../Data.Semigroup":132,"../Data.Semiring":134,"../Data.Show":136}],117:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Comonad = require("../Control.Comonad");
var Control_Extend = require("../Control.Extend");
var Control_Monad = require("../Control.Monad");
var Data_Bounded = require("../Data.Bounded");
var Data_Eq = require("../Data.Eq");
var Data_Functor = require("../Data.Functor");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Monoid = require("../Data.Monoid");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Dual = function (x) {
    return x;
};
var showDual = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Dual " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var semigroupDual = function (dictSemigroup) {
    return new Data_Semigroup.Semigroup(function (v) {
        return function (v1) {
            return Data_Semigroup.append(dictSemigroup)(v1)(v);
        };
    });
};
var runDual = function (v) {
    return v;
};
var monoidDual = function (dictMonoid) {
    return new Data_Monoid.Monoid(function () {
        return semigroupDual(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]());
    }, Data_Monoid.mempty(dictMonoid));
};
var invariantDual = new Data_Functor_Invariant.Invariant(function (f) {
    return function (v) {
        return function (v1) {
            return f(v1);
        };
    };
});
var functorDual = new Data_Functor.Functor(function (f) {
    return function (v) {
        return f(v);
    };
});
var extendDual = new Control_Extend.Extend(function () {
    return functorDual;
}, function (f) {
    return function (x) {
        return f(x);
    };
});
var eqDual = function (dictEq) {
    return new Data_Eq.Eq(function (v) {
        return function (v1) {
            return Data_Eq.eq(dictEq)(v)(v1);
        };
    });
};
var ordDual = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqDual(dictOrd["__superclass_Data.Eq.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Data_Ord.compare(dictOrd)(v)(v1);
        };
    });
};
var comonadDual = new Control_Comonad.Comonad(function () {
    return extendDual;
}, runDual);
var boundedDual = function (dictBounded) {
    return new Data_Bounded.Bounded(function () {
        return ordDual(dictBounded["__superclass_Data.Ord.Ord_0"]());
    }, Data_Bounded.bottom(dictBounded), Data_Bounded.top(dictBounded));
};
var applyDual = new Control_Apply.Apply(function () {
    return functorDual;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindDual = new Control_Bind.Bind(function () {
    return applyDual;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeDual = new Control_Applicative.Applicative(function () {
    return applyDual;
}, Dual);
var monadDual = new Control_Monad.Monad(function () {
    return applicativeDual;
}, function () {
    return bindDual;
});
module.exports = {
    Dual: Dual, 
    runDual: runDual, 
    eqDual: eqDual, 
    ordDual: ordDual, 
    boundedDual: boundedDual, 
    functorDual: functorDual, 
    invariantDual: invariantDual, 
    applyDual: applyDual, 
    applicativeDual: applicativeDual, 
    bindDual: bindDual, 
    monadDual: monadDual, 
    extendDual: extendDual, 
    comonadDual: comonadDual, 
    showDual: showDual, 
    semigroupDual: semigroupDual, 
    monoidDual: monoidDual
};

},{"../Control.Applicative":7,"../Control.Apply":9,"../Control.Bind":13,"../Control.Comonad":15,"../Control.Extend":16,"../Control.Monad":48,"../Data.Bounded":74,"../Data.Eq":79,"../Data.Functor":100,"../Data.Functor.Invariant":98,"../Data.Monoid":120,"../Data.Ord":127,"../Data.Semigroup":132,"../Data.Show":136}],118:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Data_Function = require("../Data.Function");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Monoid = require("../Data.Monoid");
var Data_Semigroup = require("../Data.Semigroup");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Control_Category = require("../Control.Category");
var Endo = function (x) {
    return x;
};
var semigroupEndo = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        return function ($10) {
            return v(v1($10));
        };
    };
});
var runEndo = function (v) {
    return v;
};
var monoidEndo = new Data_Monoid.Monoid(function () {
    return semigroupEndo;
}, Control_Category.id(Control_Category.categoryFn));
var invariantEndo = new Data_Functor_Invariant.Invariant(function (ab) {
    return function (ba) {
        return function (v) {
            return function ($11) {
                return ab(v(ba($11)));
            };
        };
    };
});
module.exports = {
    Endo: Endo, 
    runEndo: runEndo, 
    invariantEndo: invariantEndo, 
    semigroupEndo: semigroupEndo, 
    monoidEndo: monoidEndo
};

},{"../Control.Category":14,"../Control.Semigroupoid":53,"../Data.Function":97,"../Data.Functor.Invariant":98,"../Data.Monoid":120,"../Data.Semigroup":132}],119:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Comonad = require("../Control.Comonad");
var Control_Extend = require("../Control.Extend");
var Control_Monad = require("../Control.Monad");
var Data_Bounded = require("../Data.Bounded");
var Data_Eq = require("../Data.Eq");
var Data_Functor = require("../Data.Functor");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Monoid = require("../Data.Monoid");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Multiplicative = function (x) {
    return x;
};
var showMultiplicative = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Multiplicative " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var semigroupMultiplicative = function (dictSemiring) {
    return new Data_Semigroup.Semigroup(function (v) {
        return function (v1) {
            return Data_Semiring.mul(dictSemiring)(v)(v1);
        };
    });
};
var runMultiplicative = function (v) {
    return v;
};
var monoidMultiplicative = function (dictSemiring) {
    return new Data_Monoid.Monoid(function () {
        return semigroupMultiplicative(dictSemiring);
    }, Data_Semiring.one(dictSemiring));
};
var invariantMultiplicative = new Data_Functor_Invariant.Invariant(function (f) {
    return function (v) {
        return function (v1) {
            return f(v1);
        };
    };
});
var functorMultiplicative = new Data_Functor.Functor(function (f) {
    return function (v) {
        return f(v);
    };
});
var extendMultiplicative = new Control_Extend.Extend(function () {
    return functorMultiplicative;
}, function (f) {
    return function (x) {
        return f(x);
    };
});
var eqMultiplicative = function (dictEq) {
    return new Data_Eq.Eq(function (v) {
        return function (v1) {
            return Data_Eq.eq(dictEq)(v)(v1);
        };
    });
};
var ordMultiplicative = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqMultiplicative(dictOrd["__superclass_Data.Eq.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Data_Ord.compare(dictOrd)(v)(v1);
        };
    });
};
var comonadMultiplicative = new Control_Comonad.Comonad(function () {
    return extendMultiplicative;
}, runMultiplicative);
var boundedMultiplicative = function (dictBounded) {
    return new Data_Bounded.Bounded(function () {
        return ordMultiplicative(dictBounded["__superclass_Data.Ord.Ord_0"]());
    }, Data_Bounded.bottom(dictBounded), Data_Bounded.top(dictBounded));
};
var applyMultiplicative = new Control_Apply.Apply(function () {
    return functorMultiplicative;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindMultiplicative = new Control_Bind.Bind(function () {
    return applyMultiplicative;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeMultiplicative = new Control_Applicative.Applicative(function () {
    return applyMultiplicative;
}, Multiplicative);
var monadMultiplicative = new Control_Monad.Monad(function () {
    return applicativeMultiplicative;
}, function () {
    return bindMultiplicative;
});
module.exports = {
    Multiplicative: Multiplicative, 
    runMultiplicative: runMultiplicative, 
    eqMultiplicative: eqMultiplicative, 
    ordMultiplicative: ordMultiplicative, 
    boundedMultiplicative: boundedMultiplicative, 
    functorMultiplicative: functorMultiplicative, 
    invariantMultiplicative: invariantMultiplicative, 
    applyMultiplicative: applyMultiplicative, 
    applicativeMultiplicative: applicativeMultiplicative, 
    bindMultiplicative: bindMultiplicative, 
    monadMultiplicative: monadMultiplicative, 
    extendMultiplicative: extendMultiplicative, 
    comonadMultiplicative: comonadMultiplicative, 
    showMultiplicative: showMultiplicative, 
    semigroupMultiplicative: semigroupMultiplicative, 
    monoidMultiplicative: monoidMultiplicative
};

},{"../Control.Applicative":7,"../Control.Apply":9,"../Control.Bind":13,"../Control.Comonad":15,"../Control.Extend":16,"../Control.Monad":48,"../Data.Bounded":74,"../Data.Eq":79,"../Data.Functor":100,"../Data.Functor.Invariant":98,"../Data.Monoid":120,"../Data.Ord":127,"../Data.Semigroup":132,"../Data.Semiring":134,"../Data.Show":136}],120:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Data_Function = require("../Data.Function");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Unit = require("../Data.Unit");
var Monoid = function (__superclass_Data$dotSemigroup$dotSemigroup_0, mempty) {
    this["__superclass_Data.Semigroup.Semigroup_0"] = __superclass_Data$dotSemigroup$dotSemigroup_0;
    this.mempty = mempty;
};
var monoidUnit = new Monoid(function () {
    return Data_Semigroup.semigroupUnit;
}, Data_Unit.unit);
var monoidString = new Monoid(function () {
    return Data_Semigroup.semigroupString;
}, "");
var monoidArray = new Monoid(function () {
    return Data_Semigroup.semigroupArray;
}, [  ]);
var mempty = function (dict) {
    return dict.mempty;
};
var monoidFn = function (dictMonoid) {
    return new Monoid(function () {
        return Data_Semigroup.semigroupFn(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]());
    }, Data_Function["const"](mempty(dictMonoid)));
};
module.exports = {
    Monoid: Monoid, 
    mempty: mempty, 
    monoidUnit: monoidUnit, 
    monoidFn: monoidFn, 
    monoidString: monoidString, 
    monoidArray: monoidArray
};

},{"../Data.Function":97,"../Data.Semigroup":132,"../Data.Unit":151}],121:[function(require,module,exports){
arguments[4][65][0].apply(exports,arguments)
},{"dup":65}],122:[function(require,module,exports){
/* global exports */
"use strict";

exports["null"] = null;

exports.nullable = function(a, r, f) {
    return a == null ? r : f(a);
};

exports.notNull = function(x) {
    return x;
};

},{}],123:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Function = require("../Data.Function");
var Data_Function_Uncurried = require("../Data.Function.Uncurried");
var Data_Maybe = require("../Data.Maybe");
var Data_Show = require("../Data.Show");
var Data_Eq = require("../Data.Eq");
var Data_Ord = require("../Data.Ord");
var toNullable = Data_Maybe.maybe($foreign["null"])($foreign.notNull);
var toMaybe = function (n) {
    return $foreign.nullable(n, Data_Maybe.Nothing.value, Data_Maybe.Just.create);
};
var showNullable = function (dictShow) {
    return new Data_Show.Show(function (n) {
        var $3 = toMaybe(n);
        if ($3 instanceof Data_Maybe.Nothing) {
            return "null";
        };
        if ($3 instanceof Data_Maybe.Just) {
            return Data_Show.show(dictShow)($3.value0);
        };
        throw new Error("Failed pattern match at Data.Nullable line 39, column 12 - line 41, column 30: " + [ $3.constructor.name ]);
    });
};
var eqNullable = function (dictEq) {
    return new Data_Eq.Eq(Data_Function.on(Data_Eq.eq(Data_Maybe.eqMaybe(dictEq)))(toMaybe));
};
var ordNullable = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqNullable(dictOrd["__superclass_Data.Eq.Eq_0"]());
    }, Data_Function.on(Data_Ord.compare(Data_Maybe.ordMaybe(dictOrd)))(toMaybe));
};
module.exports = {
    toMaybe: toMaybe, 
    toNullable: toNullable, 
    showNullable: showNullable, 
    eqNullable: eqNullable, 
    ordNullable: ordNullable
};

},{"../Data.Eq":79,"../Data.Function":97,"../Data.Function.Uncurried":96,"../Data.Maybe":113,"../Data.Ord":127,"../Data.Show":136,"../Prelude":162,"./foreign":122}],124:[function(require,module,exports){
"use strict";

// module Data.Ord.Unsafe

exports.unsafeCompareImpl = function (lt) {
  return function (eq) {
    return function (gt) {
      return function (x) {
        return function (y) {
          return x < y ? lt : x > y ? gt : eq;
        };
      };
    };
  };
};

},{}],125:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Data_Ordering = require("../Data.Ordering");
var unsafeCompare = $foreign.unsafeCompareImpl(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value);
module.exports = {
    unsafeCompare: unsafeCompare
};

},{"../Data.Ordering":128,"./foreign":124}],126:[function(require,module,exports){
"use strict";

// module Data.Ord

exports.ordArrayImpl = function (f) {
  return function (xs) {
    return function (ys) {
      var i = 0;
      var xlen = xs.length;
      var ylen = ys.length;
      while (i < xlen && i < ylen) {
        var x = xs[i];
        var y = ys[i];
        var o = f(x)(y);
        if (o !== 0) {
          return o;
        }
        i++;
      }
      if (xlen === ylen) {
        return 0;
      } else if (xlen > ylen) {
        return -1;
      } else {
        return 1;
      }
    };
  };
};

},{}],127:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Ord_Unsafe = require("../Data.Ord.Unsafe");
var Data_Ordering = require("../Data.Ordering");
var Data_Ring = require("../Data.Ring");
var Data_Unit = require("../Data.Unit");
var Data_Void = require("../Data.Void");
var Data_Semiring = require("../Data.Semiring");
var Ord = function (__superclass_Data$dotEq$dotEq_0, compare) {
    this["__superclass_Data.Eq.Eq_0"] = __superclass_Data$dotEq$dotEq_0;
    this.compare = compare;
};
var ordVoid = new Ord(function () {
    return Data_Eq.eqVoid;
}, function (v) {
    return function (v1) {
        return Data_Ordering.EQ.value;
    };
});
var ordUnit = new Ord(function () {
    return Data_Eq.eqUnit;
}, function (v) {
    return function (v1) {
        return Data_Ordering.EQ.value;
    };
});
var ordString = new Ord(function () {
    return Data_Eq.eqString;
}, Data_Ord_Unsafe.unsafeCompare);
var ordOrdering = new Ord(function () {
    return Data_Ordering.eqOrdering;
}, function (v) {
    return function (v1) {
        if (v instanceof Data_Ordering.LT && v1 instanceof Data_Ordering.LT) {
            return Data_Ordering.EQ.value;
        };
        if (v instanceof Data_Ordering.EQ && v1 instanceof Data_Ordering.EQ) {
            return Data_Ordering.EQ.value;
        };
        if (v instanceof Data_Ordering.GT && v1 instanceof Data_Ordering.GT) {
            return Data_Ordering.EQ.value;
        };
        if (v instanceof Data_Ordering.LT) {
            return Data_Ordering.LT.value;
        };
        if (v instanceof Data_Ordering.EQ && v1 instanceof Data_Ordering.LT) {
            return Data_Ordering.GT.value;
        };
        if (v instanceof Data_Ordering.EQ && v1 instanceof Data_Ordering.GT) {
            return Data_Ordering.LT.value;
        };
        if (v instanceof Data_Ordering.GT) {
            return Data_Ordering.GT.value;
        };
        throw new Error("Failed pattern match at Data.Ord line 68, column 3 - line 68, column 21: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var ordNumber = new Ord(function () {
    return Data_Eq.eqNumber;
}, Data_Ord_Unsafe.unsafeCompare);
var ordInt = new Ord(function () {
    return Data_Eq.eqInt;
}, Data_Ord_Unsafe.unsafeCompare);
var ordChar = new Ord(function () {
    return Data_Eq.eqChar;
}, Data_Ord_Unsafe.unsafeCompare);
var ordBoolean = new Ord(function () {
    return Data_Eq.eqBoolean;
}, Data_Ord_Unsafe.unsafeCompare);
var compare = function (dict) {
    return dict.compare;
};
var comparing = function (dictOrd) {
    return function (f) {
        return Data_Function.on(compare(dictOrd))(f);
    };
};
var greaterThan = function (dictOrd) {
    return function (a1) {
        return function (a2) {
            var $22 = compare(dictOrd)(a1)(a2);
            if ($22 instanceof Data_Ordering.GT) {
                return true;
            };
            return false;
        };
    };
};
var greaterThanOrEq = function (dictOrd) {
    return function (a1) {
        return function (a2) {
            var $23 = compare(dictOrd)(a1)(a2);
            if ($23 instanceof Data_Ordering.LT) {
                return false;
            };
            return true;
        };
    };
};
var signum = function (dictOrd) {
    return function (dictRing) {
        return function (x) {
            var $24 = greaterThanOrEq(dictOrd)(x)(Data_Semiring.zero(dictRing["__superclass_Data.Semiring.Semiring_0"]()));
            if ($24) {
                return Data_Semiring.one(dictRing["__superclass_Data.Semiring.Semiring_0"]());
            };
            if (!$24) {
                return Data_Ring.negate(dictRing)(Data_Semiring.one(dictRing["__superclass_Data.Semiring.Semiring_0"]()));
            };
            throw new Error("Failed pattern match at Data.Ord line 163, column 12 - line 163, column 46: " + [ $24.constructor.name ]);
        };
    };
};
var lessThan = function (dictOrd) {
    return function (a1) {
        return function (a2) {
            var $25 = compare(dictOrd)(a1)(a2);
            if ($25 instanceof Data_Ordering.LT) {
                return true;
            };
            return false;
        };
    };
};
var lessThanOrEq = function (dictOrd) {
    return function (a1) {
        return function (a2) {
            var $26 = compare(dictOrd)(a1)(a2);
            if ($26 instanceof Data_Ordering.GT) {
                return false;
            };
            return true;
        };
    };
};
var max = function (dictOrd) {
    return function (x) {
        return function (y) {
            var $27 = compare(dictOrd)(x)(y);
            if ($27 instanceof Data_Ordering.LT) {
                return y;
            };
            if ($27 instanceof Data_Ordering.EQ) {
                return x;
            };
            if ($27 instanceof Data_Ordering.GT) {
                return x;
            };
            throw new Error("Failed pattern match at Data.Ord line 122, column 3 - line 125, column 12: " + [ $27.constructor.name ]);
        };
    };
};
var min = function (dictOrd) {
    return function (x) {
        return function (y) {
            var $28 = compare(dictOrd)(x)(y);
            if ($28 instanceof Data_Ordering.LT) {
                return x;
            };
            if ($28 instanceof Data_Ordering.EQ) {
                return x;
            };
            if ($28 instanceof Data_Ordering.GT) {
                return y;
            };
            throw new Error("Failed pattern match at Data.Ord line 113, column 3 - line 116, column 12: " + [ $28.constructor.name ]);
        };
    };
};
var ordArray = function (dictOrd) {
    return new Ord(function () {
        return Data_Eq.eqArray(dictOrd["__superclass_Data.Eq.Eq_0"]());
    }, (function () {
        var toDelta = function (x) {
            return function (y) {
                var $29 = compare(dictOrd)(x)(y);
                if ($29 instanceof Data_Ordering.EQ) {
                    return 0;
                };
                if ($29 instanceof Data_Ordering.LT) {
                    return 1;
                };
                if ($29 instanceof Data_Ordering.GT) {
                    return -1;
                };
                throw new Error("Failed pattern match at Data.Ord line 60, column 7 - line 65, column 1: " + [ $29.constructor.name ]);
            };
        };
        return function (xs) {
            return function (ys) {
                return compare(ordInt)(0)($foreign.ordArrayImpl(toDelta)(xs)(ys));
            };
        };
    })());
};
var clamp = function (dictOrd) {
    return function (low) {
        return function (hi) {
            return function (x) {
                return min(dictOrd)(hi)(max(dictOrd)(low)(x));
            };
        };
    };
};
var between = function (dictOrd) {
    return function (low) {
        return function (hi) {
            return function (x) {
                if (lessThan(dictOrd)(x)(low)) {
                    return false;
                };
                if (greaterThan(dictOrd)(x)(hi)) {
                    return false;
                };
                if (true) {
                    return true;
                };
                throw new Error("Failed pattern match at Data.Ord line 150, column 1 - line 153, column 16: " + [ low.constructor.name, hi.constructor.name, x.constructor.name ]);
            };
        };
    };
};
var abs = function (dictOrd) {
    return function (dictRing) {
        return function (x) {
            var $33 = greaterThanOrEq(dictOrd)(x)(Data_Semiring.zero(dictRing["__superclass_Data.Semiring.Semiring_0"]()));
            if ($33) {
                return x;
            };
            if (!$33) {
                return Data_Ring.negate(dictRing)(x);
            };
            throw new Error("Failed pattern match at Data.Ord line 158, column 9 - line 158, column 42: " + [ $33.constructor.name ]);
        };
    };
};
module.exports = {
    Ord: Ord, 
    abs: abs, 
    between: between, 
    clamp: clamp, 
    compare: compare, 
    comparing: comparing, 
    greaterThan: greaterThan, 
    greaterThanOrEq: greaterThanOrEq, 
    lessThan: lessThan, 
    lessThanOrEq: lessThanOrEq, 
    max: max, 
    min: min, 
    signum: signum, 
    ordBoolean: ordBoolean, 
    ordInt: ordInt, 
    ordNumber: ordNumber, 
    ordString: ordString, 
    ordChar: ordChar, 
    ordUnit: ordUnit, 
    ordVoid: ordVoid, 
    ordArray: ordArray, 
    ordOrdering: ordOrdering
};

},{"../Data.Eq":79,"../Data.Function":97,"../Data.Ord.Unsafe":125,"../Data.Ordering":128,"../Data.Ring":130,"../Data.Semiring":134,"../Data.Unit":151,"../Data.Void":152,"./foreign":126}],128:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Data_Eq = require("../Data.Eq");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var LT = (function () {
    function LT() {

    };
    LT.value = new LT();
    return LT;
})();
var GT = (function () {
    function GT() {

    };
    GT.value = new GT();
    return GT;
})();
var EQ = (function () {
    function EQ() {

    };
    EQ.value = new EQ();
    return EQ;
})();
var showOrdering = new Data_Show.Show(function (v) {
    if (v instanceof LT) {
        return "LT";
    };
    if (v instanceof GT) {
        return "GT";
    };
    if (v instanceof EQ) {
        return "EQ";
    };
    throw new Error("Failed pattern match at Data.Ordering line 27, column 3 - line 28, column 3: " + [ v.constructor.name ]);
});
var semigroupOrdering = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        if (v instanceof LT) {
            return LT.value;
        };
        if (v instanceof GT) {
            return GT.value;
        };
        if (v instanceof EQ) {
            return v1;
        };
        throw new Error("Failed pattern match at Data.Ordering line 22, column 3 - line 22, column 19: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var invert = function (v) {
    if (v instanceof GT) {
        return LT.value;
    };
    if (v instanceof EQ) {
        return EQ.value;
    };
    if (v instanceof LT) {
        return GT.value;
    };
    throw new Error("Failed pattern match at Data.Ordering line 34, column 1 - line 34, column 15: " + [ v.constructor.name ]);
};
var eqOrdering = new Data_Eq.Eq(function (v) {
    return function (v1) {
        if (v instanceof LT && v1 instanceof LT) {
            return true;
        };
        if (v instanceof GT && v1 instanceof GT) {
            return true;
        };
        if (v instanceof EQ && v1 instanceof EQ) {
            return true;
        };
        return false;
    };
});
module.exports = {
    LT: LT, 
    GT: GT, 
    EQ: EQ, 
    invert: invert, 
    eqOrdering: eqOrdering, 
    semigroupOrdering: semigroupOrdering, 
    showOrdering: showOrdering
};

},{"../Data.Eq":79,"../Data.Semigroup":132,"../Data.Show":136}],129:[function(require,module,exports){
"use strict";

// module Data.Ring

exports.intSub = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x - y | 0;
  };
};

exports.numSub = function (n1) {
  return function (n2) {
    return n1 - n2;
  };
};

},{}],130:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Data_Semiring = require("../Data.Semiring");
var Data_Unit = require("../Data.Unit");
var Ring = function (__superclass_Data$dotSemiring$dotSemiring_0, sub) {
    this["__superclass_Data.Semiring.Semiring_0"] = __superclass_Data$dotSemiring$dotSemiring_0;
    this.sub = sub;
};
var sub = function (dict) {
    return dict.sub;
};
var ringUnit = new Ring(function () {
    return Data_Semiring.semiringUnit;
}, function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
});
var ringNumber = new Ring(function () {
    return Data_Semiring.semiringNumber;
}, $foreign.numSub);
var ringInt = new Ring(function () {
    return Data_Semiring.semiringInt;
}, $foreign.intSub);
var negate = function (dictRing) {
    return function (a) {
        return sub(dictRing)(Data_Semiring.zero(dictRing["__superclass_Data.Semiring.Semiring_0"]()))(a);
    };
};
module.exports = {
    Ring: Ring, 
    negate: negate, 
    sub: sub, 
    ringInt: ringInt, 
    ringNumber: ringNumber, 
    ringUnit: ringUnit
};

},{"../Data.Semiring":134,"../Data.Unit":151,"./foreign":129}],131:[function(require,module,exports){
"use strict";

// module Data.Semigroup

exports.concatString = function (s1) {
  return function (s2) {
    return s1 + s2;
  };
};

exports.concatArray = function (xs) {
  return function (ys) {
    return xs.concat(ys);
  };
};

},{}],132:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Data_Unit = require("../Data.Unit");
var Data_Void = require("../Data.Void");
var Semigroup = function (append) {
    this.append = append;
};
var semigroupVoid = new Semigroup(function (v) {
    return Data_Void.absurd;
});
var semigroupUnit = new Semigroup(function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
});
var semigroupString = new Semigroup($foreign.concatString);
var semigroupArray = new Semigroup($foreign.concatArray);
var append = function (dict) {
    return dict.append;
};
var semigroupFn = function (dictSemigroup) {
    return new Semigroup(function (f) {
        return function (g) {
            return function (x) {
                return append(dictSemigroup)(f(x))(g(x));
            };
        };
    });
};
module.exports = {
    Semigroup: Semigroup, 
    append: append, 
    semigroupString: semigroupString, 
    semigroupUnit: semigroupUnit, 
    semigroupVoid: semigroupVoid, 
    semigroupFn: semigroupFn, 
    semigroupArray: semigroupArray
};

},{"../Data.Unit":151,"../Data.Void":152,"./foreign":131}],133:[function(require,module,exports){
"use strict";

// module Data.Semiring

exports.intAdd = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x + y | 0;
  };
};

exports.intMul = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x * y | 0;
  };
};

exports.numAdd = function (n1) {
  return function (n2) {
    return n1 + n2;
  };
};

exports.numMul = function (n1) {
  return function (n2) {
    return n1 * n2;
  };
};

},{}],134:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Data_Unit = require("../Data.Unit");
var Semiring = function (add, mul, one, zero) {
    this.add = add;
    this.mul = mul;
    this.one = one;
    this.zero = zero;
};
var zero = function (dict) {
    return dict.zero;
};
var semiringUnit = new Semiring(function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
}, function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
}, Data_Unit.unit, Data_Unit.unit);
var semiringNumber = new Semiring($foreign.numAdd, $foreign.numMul, 1.0, 0.0);
var semiringInt = new Semiring($foreign.intAdd, $foreign.intMul, 1, 0);
var one = function (dict) {
    return dict.one;
};
var mul = function (dict) {
    return dict.mul;
};
var add = function (dict) {
    return dict.add;
};
module.exports = {
    Semiring: Semiring, 
    add: add, 
    mul: mul, 
    one: one, 
    zero: zero, 
    semiringInt: semiringInt, 
    semiringNumber: semiringNumber, 
    semiringUnit: semiringUnit
};

},{"../Data.Unit":151,"./foreign":133}],135:[function(require,module,exports){
"use strict";

// module Data.Show

exports.showIntImpl = function (n) {
  return n.toString();
};

exports.showNumberImpl = function (n) {
  var str = n.toString();
  return isNaN(str + ".0") ? str : str + ".0";
};

exports.showCharImpl = function (c) {
  var code = c.charCodeAt(0);
  if (code < 0x20 || code === 0x7F) {
    switch (c) {
      case "\x07": return "'\\a'";
      case "\b": return "'\\b'";
      case "\f": return "'\\f'";
      case "\n": return "'\\n'";
      case "\r": return "'\\r'";
      case "\t": return "'\\t'";
      case "\v": return "'\\v'";
    }
    return "'\\" + code.toString(10) + "'";
  }
  return c === "'" || c === "\\" ? "'\\" + c + "'" : "'" + c + "'";
};

exports.showStringImpl = function (s) {
  var l = s.length;
  return "\"" + s.replace(
    /[\0-\x1F\x7F"\\]/g,
    function (c, i) { // jshint ignore:line
      switch (c) {
        case "\"":
        case "\\":
          return "\\" + c;
        case "\x07": return "\\a";
        case "\b": return "\\b";
        case "\f": return "\\f";
        case "\n": return "\\n";
        case "\r": return "\\r";
        case "\t": return "\\t";
        case "\v": return "\\v";
      }
      var k = i + 1;
      var empty = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
      return "\\" + c.charCodeAt(0).toString(10) + empty;
    }
  ) + "\"";
};

exports.showArrayImpl = function (f) {
  return function (xs) {
    var ss = [];
    for (var i = 0, l = xs.length; i < l; i++) {
      ss[i] = f(xs[i]);
    }
    return "[" + ss.join(",") + "]";
  };
};

},{}],136:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Show = function (show) {
    this.show = show;
};
var showString = new Show($foreign.showStringImpl);
var showNumber = new Show($foreign.showNumberImpl);
var showInt = new Show($foreign.showIntImpl);
var showChar = new Show($foreign.showCharImpl);
var showBoolean = new Show(function (v) {
    if (v) {
        return "true";
    };
    if (!v) {
        return "false";
    };
    throw new Error("Failed pattern match at Data.Show line 13, column 3 - line 14, column 3: " + [ v.constructor.name ]);
});
var show = function (dict) {
    return dict.show;
};
var showArray = function (dictShow) {
    return new Show($foreign.showArrayImpl(show(dictShow)));
};
module.exports = {
    Show: Show, 
    show: show, 
    showBoolean: showBoolean, 
    showInt: showInt, 
    showNumber: showNumber, 
    showChar: showChar, 
    showString: showString, 
    showArray: showArray
};

},{"./foreign":135}],137:[function(require,module,exports){
/* global exports */
"use strict";

// module Data.StrMap.ST

exports["new"] = function () {
  return {};
};

exports.peekImpl = function (just) {
  return function (nothing) {
    return function (m) {
      return function (k) {
        return function () {
          return {}.hasOwnProperty.call(m, k) ? just(m[k]) : nothing;
        };
      };
    };
  };
};

exports.poke = function (m) {
  return function (k) {
    return function (v) {
      return function () {
        m[k] = v;
        return m;
      };
    };
  };
};

exports["delete"] = function (m) {
  return function (k) {
    return function () {
      delete m[k];
      return m;
    };
  };
};

},{}],138:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_ST = require("../Control.Monad.ST");
var Data_Maybe = require("../Data.Maybe");
var peek = $foreign.peekImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
module.exports = {
    peek: peek, 
    "delete": $foreign["delete"], 
    "new": $foreign["new"], 
    poke: $foreign.poke
};

},{"../Control.Monad.Eff":34,"../Control.Monad.ST":43,"../Data.Maybe":113,"./foreign":137}],139:[function(require,module,exports){
/* global exports */
"use strict";

// module Data.StrMap

exports._copy = function (m) {
  var r = {};
  for (var k in m) {
    if (m.hasOwnProperty(k)) {
      r[k] = m[k];
    }
  }
  return r;
};

exports._copyEff = function (m) {
  return function () {
    var r = {};
    for (var k in m) {
      if (m.hasOwnProperty(k)) {
        r[k] = m[k];
      }
    }
    return r;
  };
};

exports.empty = {};

exports.runST = function (f) {
  return f;
};

// jshint maxparams: 2
exports._fmapStrMap = function (m0, f) {
  var m = {};
  for (var k in m0) {
    if (m0.hasOwnProperty(k)) {
      m[k] = f(m0[k]);
    }
  }
  return m;
};

// jshint maxparams: 1
exports._foldM = function (bind) {
  return function (f) {
    return function (mz) {
      return function (m) {
        var acc = mz;
        function g(k) {
          return function (z) {
            return f(z)(k)(m[k]);
          };
        }
        for (var k in m) {
          if (m.hasOwnProperty(k)) {
            acc = bind(acc)(g(k));
          }
        }
        return acc;
      };
    };
  };
};

// jshint maxparams: 4
exports._foldSCStrMap = function (m, z, f, fromMaybe) {
  for (var k in m) {
    if (m.hasOwnProperty(k)) {
      var maybeR = f(z)(k)(m[k]);
      var r = fromMaybe(null)(maybeR);
      if (r === null) return z;
      else z = r;
    }
  }
  return z;
};

// jshint maxparams: 1
exports.all = function (f) {
  return function (m) {
    for (var k in m) {
      if (m.hasOwnProperty(k) && !f(k)(m[k])) return false;
    }
    return true;
  };
};

exports.size = function (m) {
  var s = 0;
  for (var k in m) {
    if (m.hasOwnProperty(k)) {
      ++s;
    }
  }
  return s;
};

// jshint maxparams: 4
exports._lookup = function (no, yes, k, m) {
  return k in m ? yes(m[k]) : no;
};

// jshint maxparams: 2
exports._unsafeDeleteStrMap = function (m, k) {
  delete m[k];
  return m;
};

// jshint maxparams: 4
exports._lookupST = function (no, yes, k, m) {
  return function () {
    return k in m ? yes(m[k]) : no;
  };
};

function _collect(f) {
  return function (m) {
    var r = [];
    for (var k in m) {
      if (m.hasOwnProperty(k)) {
        r.push(f(k)(m[k]));
      }
    }
    return r;
  };
}

exports._collect = _collect;

exports.keys = Object.keys || _collect(function (k) {
  return function () { return k; };
});

},{}],140:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_ST = require("../Control.Monad.ST");
var Data_Foldable = require("../Data.Foldable");
var Data_Function_Uncurried = require("../Data.Function.Uncurried");
var Data_List = require("../Data.List");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_StrMap_ST = require("../Data.StrMap.ST");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Data_Functor = require("../Data.Functor");
var Data_Function = require("../Data.Function");
var Control_Apply = require("../Control.Apply");
var Control_Applicative = require("../Control.Applicative");
var Control_Category = require("../Control.Category");
var Data_Eq = require("../Data.Eq");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Show = require("../Data.Show");
var Data_Semigroup = require("../Data.Semigroup");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Control_Bind = require("../Control.Bind");
var values = function ($38) {
    return Data_List.fromFoldable(Data_Foldable.foldableArray)($foreign._collect(function (v) {
        return function (v1) {
            return v1;
        };
    })($38));
};
var toList = function ($39) {
    return Data_List.fromFoldable(Data_Foldable.foldableArray)($foreign._collect(Data_Tuple.Tuple.create)($39));
};
var thawST = $foreign._copyEff;
var showStrMap = function (dictShow) {
    return new Data_Show.Show(function (m) {
        return "fromList " + Data_Show.show(Data_List.showList(Data_Tuple.showTuple(Data_Show.showString)(dictShow)))(toList(m));
    });
};
var pureST = function (f) {
    return Control_Monad_Eff.runPure($foreign.runST(f));
};
var singleton = function (k) {
    return function (v) {
        return pureST(function __do() {
            var v1 = Data_StrMap_ST["new"]();
            Data_StrMap_ST.poke(v1)(k)(v)();
            return v1;
        });
    };
};
var mutate = function (f) {
    return function (m) {
        return pureST(function __do() {
            var v = thawST(m)();
            f(v)();
            return v;
        });
    };
};
var member = Data_Function_Uncurried.runFn4($foreign._lookup)(false)(Data_Function["const"](true));
var lookup = Data_Function_Uncurried.runFn4($foreign._lookup)(Data_Maybe.Nothing.value)(Data_Maybe.Just.create);
var isSubmap = function (dictEq) {
    return function (m1) {
        return function (m2) {
            var f = function (k) {
                return function (v) {
                    return $foreign._lookup(false, Data_Eq.eq(dictEq)(v), k, m2);
                };
            };
            return $foreign.all(f)(m1);
        };
    };
};
var isEmpty = $foreign.all(function (v) {
    return function (v1) {
        return false;
    };
});
var insert = function (k) {
    return function (v) {
        return mutate(function (s) {
            return Data_StrMap_ST.poke(s)(k)(v);
        });
    };
};
var functorStrMap = new Data_Functor.Functor(function (f) {
    return function (m) {
        return $foreign._fmapStrMap(m, f);
    };
});
var fromFoldableWith = function (dictFoldable) {
    return function (f) {
        return function (l) {
            return pureST(function __do() {
                var v = Data_StrMap_ST["new"]();
                Data_Foldable.for_(Control_Monad_Eff.applicativeEff)(dictFoldable)(l)(function (v1) {
                    return Control_Bind.bind(Control_Monad_Eff.bindEff)($foreign._lookupST(v1.value1, f(v1.value1), v1.value0, v))(Data_StrMap_ST.poke(v)(v1.value0));
                })();
                return v;
            });
        };
    };
};
var fromListWith = fromFoldableWith(Data_List.foldableList);
var fromFoldable = function (dictFoldable) {
    return function (l) {
        return pureST(function __do() {
            var v = Data_StrMap_ST["new"]();
            Data_Foldable.for_(Control_Monad_Eff.applicativeEff)(dictFoldable)(l)(function (v1) {
                return Data_StrMap_ST.poke(v)(v1.value0)(v1.value1);
            })();
            return v;
        });
    };
};
var fromList = fromFoldable(Data_List.foldableList);
var freezeST = $foreign._copyEff;
var foldMaybe = function (f) {
    return function (z) {
        return function (m) {
            return $foreign._foldSCStrMap(m, z, f, Data_Maybe.fromMaybe);
        };
    };
};
var foldM = function (dictMonad) {
    return function (f) {
        return function (z) {
            return $foreign._foldM(Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]()))(f)(Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())(z));
        };
    };
};
var semigroupStrMap = function (dictSemigroup) {
    return new Data_Semigroup.Semigroup(function (m1) {
        return function (m2) {
            return mutate(function (s1) {
                return foldM(Control_Monad_Eff.monadEff)(function (s2) {
                    return function (k) {
                        return function (v2) {
                            return Data_StrMap_ST.poke(s2)(k)($foreign._lookup(v2, function (v1) {
                                return Data_Semigroup.append(dictSemigroup)(v1)(v2);
                            }, k, m2));
                        };
                    };
                })(s1)(m1);
            })(m2);
        };
    });
};
var monoidStrMap = function (dictSemigroup) {
    return new Data_Monoid.Monoid(function () {
        return semigroupStrMap(dictSemigroup);
    }, $foreign.empty);
};
var union = function (m) {
    return mutate(function (s) {
        return foldM(Control_Monad_Eff.monadEff)(Data_StrMap_ST.poke)(s)(m);
    });
};
var unions = Data_Foldable.foldl(Data_List.foldableList)(union)($foreign.empty);
var fold = $foreign._foldM(Data_Function.applyFlipped);
var foldMap = function (dictMonoid) {
    return function (f) {
        return fold(function (acc) {
            return function (k) {
                return function (v) {
                    return Data_Semigroup.append(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]())(acc)(f(k)(v));
                };
            };
        })(Data_Monoid.mempty(dictMonoid));
    };
};
var foldableStrMap = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return foldMap(dictMonoid)(Data_Function["const"](f));
    };
}, function (f) {
    return fold(function (z) {
        return function (v) {
            return f(z);
        };
    });
}, function (f) {
    return function (z) {
        return function (m) {
            return Data_Foldable.foldr(Data_List.foldableList)(f)(z)(values(m));
        };
    };
});
var traversableStrMap = new Data_Traversable.Traversable(function () {
    return foldableStrMap;
}, function () {
    return functorStrMap;
}, function (dictApplicative) {
    return Data_Traversable.traverse(traversableStrMap)(dictApplicative)(Control_Category.id(Control_Category.categoryFn));
}, function (dictApplicative) {
    return function (f) {
        return function (ms) {
            return Data_Foldable.foldr(Data_List.foldableList)(function (x) {
                return function (acc) {
                    return Control_Apply.apply(dictApplicative["__superclass_Control.Apply.Apply_0"]())(Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(union)(x))(acc);
                };
            })(Control_Applicative.pure(dictApplicative)($foreign.empty))(Data_Functor.map(Data_List.functorList)(Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Data_Tuple.uncurry(singleton)))(Data_Functor.map(Data_List.functorList)(Data_Traversable.traverse(Data_Tuple.traversableTuple)(dictApplicative)(f))(toList(ms))));
        };
    };
});
var eqStrMap = function (dictEq) {
    return new Data_Eq.Eq(function (m1) {
        return function (m2) {
            return isSubmap(dictEq)(m1)(m2) && isSubmap(dictEq)(m2)(m1);
        };
    });
};
var $$delete = function (k) {
    return mutate(function (s) {
        return Data_StrMap_ST["delete"](s)(k);
    });
};
var pop = function (k) {
    return function (m) {
        return Data_Functor.mapFlipped(Data_Maybe.functorMaybe)(lookup(k)(m))(function (a) {
            return new Data_Tuple.Tuple(a, $$delete(k)(m));
        });
    };
};
var alter = function (f) {
    return function (k) {
        return function (m) {
            var $36 = f(lookup(k)(m));
            if ($36 instanceof Data_Maybe.Nothing) {
                return $$delete(k)(m);
            };
            if ($36 instanceof Data_Maybe.Just) {
                return insert(k)($36.value0)(m);
            };
            throw new Error("Failed pattern match at Data.StrMap line 185, column 15 - line 187, column 25: " + [ $36.constructor.name ]);
        };
    };
};
var update = function (f) {
    return function (k) {
        return function (m) {
            return alter(Data_Maybe.maybe(Data_Maybe.Nothing.value)(f))(k)(m);
        };
    };
};
module.exports = {
    alter: alter, 
    "delete": $$delete, 
    fold: fold, 
    foldM: foldM, 
    foldMap: foldMap, 
    foldMaybe: foldMaybe, 
    freezeST: freezeST, 
    fromFoldable: fromFoldable, 
    fromFoldableWith: fromFoldableWith, 
    fromList: fromList, 
    fromListWith: fromListWith, 
    insert: insert, 
    isEmpty: isEmpty, 
    isSubmap: isSubmap, 
    lookup: lookup, 
    member: member, 
    pop: pop, 
    pureST: pureST, 
    singleton: singleton, 
    thawST: thawST, 
    toList: toList, 
    union: union, 
    unions: unions, 
    update: update, 
    values: values, 
    functorStrMap: functorStrMap, 
    foldableStrMap: foldableStrMap, 
    traversableStrMap: traversableStrMap, 
    eqStrMap: eqStrMap, 
    showStrMap: showStrMap, 
    semigroupStrMap: semigroupStrMap, 
    monoidStrMap: monoidStrMap, 
    all: $foreign.all, 
    empty: $foreign.empty, 
    keys: $foreign.keys, 
    runST: $foreign.runST, 
    size: $foreign.size
};

},{"../Control.Applicative":7,"../Control.Apply":9,"../Control.Bind":13,"../Control.Category":14,"../Control.Monad.Eff":34,"../Control.Monad.ST":43,"../Control.Semigroupoid":53,"../Data.Eq":79,"../Data.Foldable":84,"../Data.Function":97,"../Data.Function.Uncurried":96,"../Data.Functor":100,"../Data.HeytingAlgebra":104,"../Data.List":110,"../Data.Maybe":113,"../Data.Monoid":120,"../Data.Semigroup":132,"../Data.Show":136,"../Data.StrMap.ST":138,"../Data.Traversable":146,"../Data.Tuple":147,"../Prelude":162,"./foreign":139}],141:[function(require,module,exports){
/* global exports */
"use strict";

// module Data.String.Unsafe

exports.charCodeAt = function (i) {
  return function (s) {
    if (i >= 0 && i < s.length) return s.charCodeAt(i);
    throw new Error("Data.String.Unsafe.charCodeAt: Invalid index.");
  };
};

exports.charAt = function (i) {
  return function (s) {
    if (i >= 0 && i < s.length) return s.charAt(i);
    throw new Error("Data.String.Unsafe.charAt: Invalid index.");
  };
};

exports.char = function (s) {
  if (s.length === 1) return s.charAt(0);
  throw new Error("Data.String.Unsafe.char: Expected string of length 1.");
};

},{}],142:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
module.exports = {
    "char": $foreign["char"], 
    charAt: $foreign.charAt, 
    charCodeAt: $foreign.charCodeAt
};

},{"./foreign":141}],143:[function(require,module,exports){
/* global exports */
"use strict";

// module Data.String

exports._charAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (s) {
        return i >= 0 && i < s.length ? just(s.charAt(i)) : nothing;
      };
    };
  };
};

exports.singleton = function (c) {
  return c;
};

exports._charCodeAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (s) {
        return i >= 0 && i < s.length ? just(s.charCodeAt(i)) : nothing;
      };
    };
  };
};

exports._toChar = function (just) {
  return function (nothing) {
    return function (s) {
      return s.length === 1 ? just(s) : nothing;
    };
  };
};

exports.fromCharArray = function (a) {
  return a.join("");
};

exports._indexOf = function (just) {
  return function (nothing) {
    return function (x) {
      return function (s) {
        var i = s.indexOf(x);
        return i === -1 ? nothing : just(i);
      };
    };
  };
};

exports["_indexOf'"] = function (just) {
  return function (nothing) {
    return function (x) {
      return function (startAt) {
        return function (s) {
          if (startAt < 0 || startAt > s.length) return nothing;
          var i = s.indexOf(x, startAt);
          return i === -1 ? nothing : just(i);
        };
      };
    };
  };
};

exports._lastIndexOf = function (just) {
  return function (nothing) {
    return function (x) {
      return function (s) {
        var i = s.lastIndexOf(x);
        return i === -1 ? nothing : just(i);
      };
    };
  };
};

exports["_lastIndexOf'"] = function (just) {
  return function (nothing) {
    return function (x) {
      return function (startAt) {
        return function (s) {
          if (startAt < 0 || startAt > s.length) return nothing;
          var i = s.lastIndexOf(x, startAt);
          return i === -1 ? nothing : just(i);
        };
      };
    };
  };
};

exports.length = function (s) {
  return s.length;
};

exports._localeCompare = function (lt) {
  return function (eq) {
    return function (gt) {
      return function (s1) {
        return function (s2) {
          var result = s1.localeCompare(s2);
          return result < 0 ? lt : result > 0 ? gt : eq;
        };
      };
    };
  };
};

exports.replace = function (s1) {
  return function (s2) {
    return function (s3) {
      return s3.replace(s1, s2);
    };
  };
};

exports.take = function (n) {
  return function (s) {
    return s.substr(0, n);
  };
};

exports.drop = function (n) {
  return function (s) {
    return s.substring(n);
  };
};

exports.count = function (p) {
  return function (s) {
    for (var i = 0; i < s.length && p(s.charAt(i)); i++); {}
    return i;
  };
};

exports.split = function (sep) {
  return function (s) {
    return s.split(sep);
  };
};

exports.toCharArray = function (s) {
  return s.split("");
};

exports.toLower = function (s) {
  return s.toLowerCase();
};

exports.toUpper = function (s) {
  return s.toUpperCase();
};

exports.trim = function (s) {
  return s.trim();
};

exports.joinWith = function (s) {
  return function (xs) {
    return xs.join(s);
  };
};

},{}],144:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Maybe = require("../Data.Maybe");
var Data_String_Unsafe = require("../Data.String.Unsafe");
var Data_Semiring = require("../Data.Semiring");
var Data_Eq = require("../Data.Eq");
var Data_Ordering = require("../Data.Ordering");
var Data_Ring = require("../Data.Ring");
var Data_Function = require("../Data.Function");
var uncons = function (v) {
    if (v === "") {
        return Data_Maybe.Nothing.value;
    };
    return new Data_Maybe.Just({
        head: Data_String_Unsafe.charAt(0)(v), 
        tail: $foreign.drop(1)(v)
    });
};
var toChar = $foreign._toChar(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var takeWhile = function (p) {
    return function (s) {
        return $foreign.take($foreign.count(p)(s))(s);
    };
};
var $$null = function (s) {
    return $foreign.length(s) === 0;
};
var localeCompare = $foreign._localeCompare(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value);
var lastIndexOf$prime = $foreign["_lastIndexOf'"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var lastIndexOf = $foreign._lastIndexOf(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var stripSuffix = function (suffix) {
    return function (str) {
        var $2 = lastIndexOf(suffix)(str);
        if ($2 instanceof Data_Maybe.Just && $2.value0 === $foreign.length(str) - $foreign.length(suffix)) {
            return Data_Function.apply(Data_Maybe.Just.create)($foreign.take($2.value0)(str));
        };
        return Data_Maybe.Nothing.value;
    };
};
var indexOf$prime = $foreign["_indexOf'"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var indexOf = $foreign._indexOf(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var stripPrefix = function (prefix) {
    return function (str) {
        var $4 = indexOf(prefix)(str);
        if ($4 instanceof Data_Maybe.Just && $4.value0 === 0) {
            return Data_Function.apply(Data_Maybe.Just.create)($foreign.drop($foreign.length(prefix))(str));
        };
        return Data_Maybe.Nothing.value;
    };
};
var dropWhile = function (p) {
    return function (s) {
        return $foreign.drop($foreign.count(p)(s))(s);
    };
};
var contains = function (x) {
    return function (s) {
        return Data_Maybe.isJust(indexOf(x)(s));
    };
};
var charCodeAt = $foreign._charCodeAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var charAt = $foreign._charAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
module.exports = {
    charAt: charAt, 
    charCodeAt: charCodeAt, 
    contains: contains, 
    dropWhile: dropWhile, 
    indexOf: indexOf, 
    "indexOf'": indexOf$prime, 
    lastIndexOf: lastIndexOf, 
    "lastIndexOf'": lastIndexOf$prime, 
    localeCompare: localeCompare, 
    "null": $$null, 
    stripPrefix: stripPrefix, 
    stripSuffix: stripSuffix, 
    takeWhile: takeWhile, 
    toChar: toChar, 
    uncons: uncons, 
    count: $foreign.count, 
    drop: $foreign.drop, 
    fromCharArray: $foreign.fromCharArray, 
    joinWith: $foreign.joinWith, 
    length: $foreign.length, 
    replace: $foreign.replace, 
    singleton: $foreign.singleton, 
    split: $foreign.split, 
    take: $foreign.take, 
    toCharArray: $foreign.toCharArray, 
    toLower: $foreign.toLower, 
    toUpper: $foreign.toUpper, 
    trim: $foreign.trim
};

},{"../Data.Eq":79,"../Data.Function":97,"../Data.Maybe":113,"../Data.Ordering":128,"../Data.Ring":130,"../Data.Semiring":134,"../Data.String.Unsafe":142,"../Prelude":162,"./foreign":143}],145:[function(require,module,exports){
"use strict";

// module Data.Traversable

// jshint maxparams: 3

exports.traverseArrayImpl = function () {
  function Cont(fn) {
    this.fn = fn;
  }

  var emptyList = {};

  var ConsCell = function (head, tail) {
    this.head = head;
    this.tail = tail;
  };

  function consList(x) {
    return function (xs) {
      return new ConsCell(x, xs);
    };
  }

  function listToArray(list) {
    var arr = [];
    while (list !== emptyList) {
      arr.push(list.head);
      list = list.tail;
    }
    return arr;
  }

  return function (apply) {
    return function (map) {
      return function (pure) {
        return function (f) {
          var buildFrom = function (x, ys) {
            return apply(map(consList)(f(x)))(ys);
          };

          var go = function (acc, currentLen, xs) {
            if (currentLen === 0) {
              return acc;
            } else {
              var last = xs[currentLen - 1];
              return new Cont(function () {
                return go(buildFrom(last, acc), currentLen - 1, xs);
              });
            }
          };

          return function (array) {
            var result = go(pure(emptyList), array.length, array);
            while (result instanceof Cont) {
              result = result.fn();
            }

            return map(listToArray)(result);
          };
        };
      };
    };
  };
}();

},{}],146:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Category = require("../Control.Category");
var Data_Foldable = require("../Data.Foldable");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Data_Maybe_First = require("../Data.Maybe.First");
var Data_Maybe_Last = require("../Data.Maybe.Last");
var Data_Monoid_Additive = require("../Data.Monoid.Additive");
var Data_Monoid_Conj = require("../Data.Monoid.Conj");
var Data_Monoid_Disj = require("../Data.Monoid.Disj");
var Data_Monoid_Dual = require("../Data.Monoid.Dual");
var Data_Monoid_Multiplicative = require("../Data.Monoid.Multiplicative");
var StateL = function (x) {
    return x;
};
var StateR = function (x) {
    return x;
};
var Traversable = function (__superclass_Data$dotFoldable$dotFoldable_1, __superclass_Data$dotFunctor$dotFunctor_0, sequence, traverse) {
    this["__superclass_Data.Foldable.Foldable_1"] = __superclass_Data$dotFoldable$dotFoldable_1;
    this["__superclass_Data.Functor.Functor_0"] = __superclass_Data$dotFunctor$dotFunctor_0;
    this.sequence = sequence;
    this.traverse = traverse;
};
var traverse = function (dict) {
    return dict.traverse;
};
var traversableMultiplicative = new Traversable(function () {
    return Data_Foldable.foldableMultiplicative;
}, function () {
    return Data_Monoid_Multiplicative.functorMultiplicative;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Data_Monoid_Multiplicative.Multiplicative)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Data_Monoid_Multiplicative.Multiplicative)(f(v));
        };
    };
});
var traversableMaybe = new Traversable(function () {
    return Data_Foldable.foldableMaybe;
}, function () {
    return Data_Maybe.functorMaybe;
}, function (dictApplicative) {
    return function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return Control_Applicative.pure(dictApplicative)(Data_Maybe.Nothing.value);
        };
        if (v instanceof Data_Maybe.Just) {
            return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Data_Maybe.Just.create)(v.value0);
        };
        throw new Error("Failed pattern match at Data.Traversable line 88, column 3 - line 88, column 35: " + [ v.constructor.name ]);
    };
}, function (dictApplicative) {
    return function (v) {
        return function (v1) {
            if (v1 instanceof Data_Maybe.Nothing) {
                return Control_Applicative.pure(dictApplicative)(Data_Maybe.Nothing.value);
            };
            if (v1 instanceof Data_Maybe.Just) {
                return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Data_Maybe.Just.create)(v(v1.value0));
            };
            throw new Error("Failed pattern match at Data.Traversable line 86, column 3 - line 86, column 37: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
});
var traversableDual = new Traversable(function () {
    return Data_Foldable.foldableDual;
}, function () {
    return Data_Monoid_Dual.functorDual;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Data_Monoid_Dual.Dual)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Data_Monoid_Dual.Dual)(f(v));
        };
    };
});
var traversableDisj = new Traversable(function () {
    return Data_Foldable.foldableDisj;
}, function () {
    return Data_Monoid_Disj.functorDisj;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Data_Monoid_Disj.Disj)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Data_Monoid_Disj.Disj)(f(v));
        };
    };
});
var traversableConj = new Traversable(function () {
    return Data_Foldable.foldableConj;
}, function () {
    return Data_Monoid_Conj.functorConj;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Data_Monoid_Conj.Conj)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Data_Monoid_Conj.Conj)(f(v));
        };
    };
});
var traversableAdditive = new Traversable(function () {
    return Data_Foldable.foldableAdditive;
}, function () {
    return Data_Monoid_Additive.functorAdditive;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Data_Monoid_Additive.Additive)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Data_Monoid_Additive.Additive)(f(v));
        };
    };
});
var stateR = function (v) {
    return v;
};
var stateL = function (v) {
    return v;
};
var sequenceDefault = function (dictTraversable) {
    return function (dictApplicative) {
        return function (tma) {
            return traverse(dictTraversable)(dictApplicative)(Control_Category.id(Control_Category.categoryFn))(tma);
        };
    };
};
var traversableArray = new Traversable(function () {
    return Data_Foldable.foldableArray;
}, function () {
    return Data_Functor.functorArray;
}, function (dictApplicative) {
    return sequenceDefault(traversableArray)(dictApplicative);
}, function (dictApplicative) {
    return $foreign.traverseArrayImpl(Control_Apply.apply(dictApplicative["__superclass_Control.Apply.Apply_0"]()))(Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]()))(Control_Applicative.pure(dictApplicative));
});
var sequence = function (dict) {
    return dict.sequence;
};
var traversableFirst = new Traversable(function () {
    return Data_Foldable.foldableFirst;
}, function () {
    return Data_Maybe_First.functorFirst;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Data_Maybe_First.First)(sequence(traversableMaybe)(dictApplicative)(v));
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Data_Maybe_First.First)(traverse(traversableMaybe)(dictApplicative)(f)(v));
        };
    };
});
var traversableLast = new Traversable(function () {
    return Data_Foldable.foldableLast;
}, function () {
    return Data_Maybe_Last.functorLast;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Data_Maybe_Last.Last)(sequence(traversableMaybe)(dictApplicative)(v));
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Data_Maybe_Last.Last)(traverse(traversableMaybe)(dictApplicative)(f)(v));
        };
    };
});
var traverseDefault = function (dictTraversable) {
    return function (dictApplicative) {
        return function (f) {
            return function (ta) {
                return sequence(dictTraversable)(dictApplicative)(Data_Functor.map(dictTraversable["__superclass_Data.Functor.Functor_0"]())(f)(ta));
            };
        };
    };
};
var functorStateR = new Data_Functor.Functor(function (f) {
    return function (k) {
        return function (s) {
            var $75 = stateR(k)(s);
            return {
                accum: $75.accum, 
                value: f($75.value)
            };
        };
    };
});
var functorStateL = new Data_Functor.Functor(function (f) {
    return function (k) {
        return function (s) {
            var $78 = stateL(k)(s);
            return {
                accum: $78.accum, 
                value: f($78.value)
            };
        };
    };
});
var $$for = function (dictApplicative) {
    return function (dictTraversable) {
        return function (x) {
            return function (f) {
                return traverse(dictTraversable)(dictApplicative)(f)(x);
            };
        };
    };
};
var applyStateR = new Control_Apply.Apply(function () {
    return functorStateR;
}, function (f) {
    return function (x) {
        return function (s) {
            var $81 = stateR(x)(s);
            var $82 = stateR(f)($81.accum);
            return {
                accum: $82.accum, 
                value: $82.value($81.value)
            };
        };
    };
});
var applyStateL = new Control_Apply.Apply(function () {
    return functorStateL;
}, function (f) {
    return function (x) {
        return function (s) {
            var $87 = stateL(f)(s);
            var $88 = stateL(x)($87.accum);
            return {
                accum: $88.accum, 
                value: $87.value($88.value)
            };
        };
    };
});
var applicativeStateR = new Control_Applicative.Applicative(function () {
    return applyStateR;
}, function (a) {
    return function (s) {
        return {
            accum: s, 
            value: a
        };
    };
});
var mapAccumR = function (dictTraversable) {
    return function (f) {
        return function (s0) {
            return function (xs) {
                return stateR(traverse(dictTraversable)(applicativeStateR)(function (a) {
                    return function (s) {
                        return f(s)(a);
                    };
                })(xs))(s0);
            };
        };
    };
};
var scanr = function (dictTraversable) {
    return function (f) {
        return function (b0) {
            return function (xs) {
                return (mapAccumR(dictTraversable)(function (b) {
                    return function (a) {
                        var b$prime = f(a)(b);
                        return {
                            accum: b$prime, 
                            value: b$prime
                        };
                    };
                })(b0)(xs)).value;
            };
        };
    };
};
var applicativeStateL = new Control_Applicative.Applicative(function () {
    return applyStateL;
}, function (a) {
    return function (s) {
        return {
            accum: s, 
            value: a
        };
    };
});
var mapAccumL = function (dictTraversable) {
    return function (f) {
        return function (s0) {
            return function (xs) {
                return stateL(traverse(dictTraversable)(applicativeStateL)(function (a) {
                    return function (s) {
                        return f(s)(a);
                    };
                })(xs))(s0);
            };
        };
    };
};
var scanl = function (dictTraversable) {
    return function (f) {
        return function (b0) {
            return function (xs) {
                return (mapAccumL(dictTraversable)(function (b) {
                    return function (a) {
                        var b$prime = f(b)(a);
                        return {
                            accum: b$prime, 
                            value: b$prime
                        };
                    };
                })(b0)(xs)).value;
            };
        };
    };
};
module.exports = {
    Traversable: Traversable, 
    "for": $$for, 
    mapAccumL: mapAccumL, 
    mapAccumR: mapAccumR, 
    scanl: scanl, 
    scanr: scanr, 
    sequence: sequence, 
    sequenceDefault: sequenceDefault, 
    traverse: traverse, 
    traverseDefault: traverseDefault, 
    traversableArray: traversableArray, 
    traversableMaybe: traversableMaybe, 
    traversableFirst: traversableFirst, 
    traversableLast: traversableLast, 
    traversableAdditive: traversableAdditive, 
    traversableDual: traversableDual, 
    traversableConj: traversableConj, 
    traversableDisj: traversableDisj, 
    traversableMultiplicative: traversableMultiplicative
};

},{"../Control.Applicative":7,"../Control.Apply":9,"../Control.Category":14,"../Data.Foldable":84,"../Data.Functor":100,"../Data.Maybe":113,"../Data.Maybe.First":111,"../Data.Maybe.Last":112,"../Data.Monoid.Additive":114,"../Data.Monoid.Conj":115,"../Data.Monoid.Disj":116,"../Data.Monoid.Dual":117,"../Data.Monoid.Multiplicative":119,"./foreign":145}],147:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Biapplicative = require("../Control.Biapplicative");
var Control_Biapply = require("../Control.Biapply");
var Control_Bind = require("../Control.Bind");
var Control_Comonad = require("../Control.Comonad");
var Control_Extend = require("../Control.Extend");
var Control_Lazy = require("../Control.Lazy");
var Control_Monad = require("../Control.Monad");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Bifoldable = require("../Data.Bifoldable");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Bitraversable = require("../Data.Bitraversable");
var Data_BooleanAlgebra = require("../Data.BooleanAlgebra");
var Data_Bounded = require("../Data.Bounded");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Maybe = require("../Data.Maybe");
var Data_Maybe_First = require("../Data.Maybe.First");
var Data_Monoid = require("../Data.Monoid");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Ring = require("../Data.Ring");
var Data_CommutativeRing = require("../Data.CommutativeRing");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_Traversable = require("../Data.Traversable");
var Data_Unit = require("../Data.Unit");
var Tuple = (function () {
    function Tuple(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Tuple.create = function (value0) {
        return function (value1) {
            return new Tuple(value0, value1);
        };
    };
    return Tuple;
})();
var uncurry = function (f) {
    return function (v) {
        return f(v.value0)(v.value1);
    };
};
var swap = function (v) {
    return new Tuple(v.value1, v.value0);
};
var snd = function (v) {
    return v.value1;
};
var showTuple = function (dictShow) {
    return function (dictShow1) {
        return new Data_Show.Show(function (v) {
            return "(Tuple " + (Data_Show.show(dictShow)(v.value0) + (" " + (Data_Show.show(dictShow1)(v.value1) + ")")));
        });
    };
};
var semiringTuple = function (dictSemiring) {
    return function (dictSemiring1) {
        return new Data_Semiring.Semiring(function (v) {
            return function (v1) {
                return new Tuple(Data_Semiring.add(dictSemiring)(v.value0)(v1.value0), Data_Semiring.add(dictSemiring1)(v.value1)(v1.value1));
            };
        }, function (v) {
            return function (v1) {
                return new Tuple(Data_Semiring.mul(dictSemiring)(v.value0)(v1.value0), Data_Semiring.mul(dictSemiring1)(v.value1)(v1.value1));
            };
        }, new Tuple(Data_Semiring.one(dictSemiring), Data_Semiring.one(dictSemiring1)), new Tuple(Data_Semiring.zero(dictSemiring), Data_Semiring.zero(dictSemiring1)));
    };
};
var semigroupoidTuple = new Control_Semigroupoid.Semigroupoid(function (v) {
    return function (v1) {
        return new Tuple(v1.value0, v.value1);
    };
});
var semigroupTuple = function (dictSemigroup) {
    return function (dictSemigroup1) {
        return new Data_Semigroup.Semigroup(function (v) {
            return function (v1) {
                return new Tuple(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0), Data_Semigroup.append(dictSemigroup1)(v.value1)(v1.value1));
            };
        });
    };
};
var ringTuple = function (dictRing) {
    return function (dictRing1) {
        return new Data_Ring.Ring(function () {
            return semiringTuple(dictRing["__superclass_Data.Semiring.Semiring_0"]())(dictRing1["__superclass_Data.Semiring.Semiring_0"]());
        }, function (v) {
            return function (v1) {
                return new Tuple(Data_Ring.sub(dictRing)(v.value0)(v1.value0), Data_Ring.sub(dictRing1)(v.value1)(v1.value1));
            };
        });
    };
};
var monoidTuple = function (dictMonoid) {
    return function (dictMonoid1) {
        return new Data_Monoid.Monoid(function () {
            return semigroupTuple(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]())(dictMonoid1["__superclass_Data.Semigroup.Semigroup_0"]());
        }, new Tuple(Data_Monoid.mempty(dictMonoid), Data_Monoid.mempty(dictMonoid1)));
    };
};
var lookup = function (dictFoldable) {
    return function (dictEq) {
        return function (a) {
            return function (f) {
                return Data_Function.apply(Data_Maybe_First.runFirst)(Data_Foldable.foldMap(dictFoldable)(Data_Maybe_First.monoidFirst)(function (v) {
                    var $127 = Data_Eq.eq(dictEq)(a)(v.value0);
                    if ($127) {
                        return new Data_Maybe.Just(v.value1);
                    };
                    if (!$127) {
                        return Data_Maybe.Nothing.value;
                    };
                    throw new Error("Failed pattern match at Data.Tuple line 189, column 58 - line 189, column 93: " + [ $127.constructor.name ]);
                })(f));
            };
        };
    };
};
var heytingAlgebraTuple = function (dictHeytingAlgebra) {
    return function (dictHeytingAlgebra1) {
        return new Data_HeytingAlgebra.HeytingAlgebra(function (v) {
            return function (v1) {
                return new Tuple(Data_HeytingAlgebra.conj(dictHeytingAlgebra)(v.value0)(v1.value0), Data_HeytingAlgebra.conj(dictHeytingAlgebra1)(v.value1)(v1.value1));
            };
        }, function (v) {
            return function (v1) {
                return new Tuple(Data_HeytingAlgebra.disj(dictHeytingAlgebra)(v.value0)(v1.value0), Data_HeytingAlgebra.disj(dictHeytingAlgebra1)(v.value1)(v1.value1));
            };
        }, new Tuple(Data_HeytingAlgebra.ff(dictHeytingAlgebra), Data_HeytingAlgebra.ff(dictHeytingAlgebra1)), function (v) {
            return function (v1) {
                return new Tuple(Data_HeytingAlgebra.implies(dictHeytingAlgebra)(v.value0)(v1.value0), Data_HeytingAlgebra.implies(dictHeytingAlgebra1)(v.value1)(v1.value1));
            };
        }, function (v) {
            return new Tuple(Data_HeytingAlgebra.not(dictHeytingAlgebra)(v.value0), Data_HeytingAlgebra.not(dictHeytingAlgebra1)(v.value1));
        }, new Tuple(Data_HeytingAlgebra.tt(dictHeytingAlgebra), Data_HeytingAlgebra.tt(dictHeytingAlgebra1)));
    };
};
var functorTuple = new Data_Functor.Functor(function (f) {
    return function (v) {
        return new Tuple(v.value0, f(v.value1));
    };
});
var invariantTuple = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorTuple));
var fst = function (v) {
    return v.value0;
};
var lazyTuple = function (dictLazy) {
    return function (dictLazy1) {
        return new Control_Lazy.Lazy(function (f) {
            return new Tuple(Data_Function.apply(Control_Lazy.defer(dictLazy))(function (v) {
                return fst(f(Data_Unit.unit));
            }), Data_Function.apply(Control_Lazy.defer(dictLazy1))(function (v) {
                return snd(f(Data_Unit.unit));
            }));
        });
    };
};
var foldableTuple = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v.value1);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v.value1);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v.value1)(z);
        };
    };
});
var traversableTuple = new Data_Traversable.Traversable(function () {
    return foldableTuple;
}, function () {
    return functorTuple;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Tuple.create(v.value0))(v.value1);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Tuple.create(v.value0))(f(v.value1));
        };
    };
});
var extendTuple = new Control_Extend.Extend(function () {
    return functorTuple;
}, function (f) {
    return function (v) {
        return new Tuple(v.value0, f(v));
    };
});
var eqTuple = function (dictEq) {
    return function (dictEq1) {
        return new Data_Eq.Eq(function (v) {
            return function (v1) {
                return Data_Eq.eq(dictEq)(v.value0)(v1.value0) && Data_Eq.eq(dictEq1)(v.value1)(v1.value1);
            };
        });
    };
};
var ordTuple = function (dictOrd) {
    return function (dictOrd1) {
        return new Data_Ord.Ord(function () {
            return eqTuple(dictOrd["__superclass_Data.Eq.Eq_0"]())(dictOrd1["__superclass_Data.Eq.Eq_0"]());
        }, function (v) {
            return function (v1) {
                var $193 = Data_Ord.compare(dictOrd)(v.value0)(v1.value0);
                if ($193 instanceof Data_Ordering.EQ) {
                    return Data_Ord.compare(dictOrd1)(v.value1)(v1.value1);
                };
                return $193;
            };
        });
    };
};
var curry = function (f) {
    return function (a) {
        return function (b) {
            return f(new Tuple(a, b));
        };
    };
};
var comonadTuple = new Control_Comonad.Comonad(function () {
    return extendTuple;
}, snd);
var commutativeRingTuple = function (dictCommutativeRing) {
    return function (dictCommutativeRing1) {
        return new Data_CommutativeRing.CommutativeRing(function () {
            return ringTuple(dictCommutativeRing["__superclass_Data.Ring.Ring_0"]())(dictCommutativeRing1["__superclass_Data.Ring.Ring_0"]());
        });
    };
};
var boundedTuple = function (dictBounded) {
    return function (dictBounded1) {
        return new Data_Bounded.Bounded(function () {
            return ordTuple(dictBounded["__superclass_Data.Ord.Ord_0"]())(dictBounded1["__superclass_Data.Ord.Ord_0"]());
        }, new Tuple(Data_Bounded.bottom(dictBounded), Data_Bounded.bottom(dictBounded1)), new Tuple(Data_Bounded.top(dictBounded), Data_Bounded.top(dictBounded1)));
    };
};
var booleanAlgebraTuple = function (dictBooleanAlgebra) {
    return function (dictBooleanAlgebra1) {
        return new Data_BooleanAlgebra.BooleanAlgebra(function () {
            return heytingAlgebraTuple(dictBooleanAlgebra["__superclass_Data.HeytingAlgebra.HeytingAlgebra_0"]())(dictBooleanAlgebra1["__superclass_Data.HeytingAlgebra.HeytingAlgebra_0"]());
        });
    };
};
var bifunctorTuple = new Data_Bifunctor.Bifunctor(function (f) {
    return function (g) {
        return function (v) {
            return new Tuple(f(v.value0), g(v.value1));
        };
    };
});
var bifoldableTuple = new Data_Bifoldable.Bifoldable(function (dictMonoid) {
    return function (f) {
        return function (g) {
            return function (v) {
                return Data_Semigroup.append(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]())(f(v.value0))(g(v.value1));
            };
        };
    };
}, function (f) {
    return function (g) {
        return function (z) {
            return function (v) {
                return g(f(z)(v.value0))(v.value1);
            };
        };
    };
}, function (f) {
    return function (g) {
        return function (z) {
            return function (v) {
                return f(v.value0)(g(v.value1)(z));
            };
        };
    };
});
var bitraversableTuple = new Data_Bitraversable.Bitraversable(function () {
    return bifoldableTuple;
}, function () {
    return bifunctorTuple;
}, function (dictApplicative) {
    return function (v) {
        return Control_Apply.apply(dictApplicative["__superclass_Control.Apply.Apply_0"]())(Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Tuple.create)(v.value0))(v.value1);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (g) {
            return function (v) {
                return Control_Apply.apply(dictApplicative["__superclass_Control.Apply.Apply_0"]())(Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Tuple.create)(f(v.value0)))(g(v.value1));
            };
        };
    };
});
var biapplyTuple = new Control_Biapply.Biapply(function () {
    return bifunctorTuple;
}, function (v) {
    return function (v1) {
        return new Tuple(v.value0(v1.value0), v.value1(v1.value1));
    };
});
var biapplicativeTuple = new Control_Biapplicative.Biapplicative(function () {
    return biapplyTuple;
}, Tuple.create);
var applyTuple = function (dictSemigroup) {
    return new Control_Apply.Apply(function () {
        return functorTuple;
    }, function (v) {
        return function (v1) {
            return new Tuple(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0), v.value1(v1.value1));
        };
    });
};
var bindTuple = function (dictSemigroup) {
    return new Control_Bind.Bind(function () {
        return applyTuple(dictSemigroup);
    }, function (v) {
        return function (f) {
            var $242 = f(v.value1);
            return new Tuple(Data_Semigroup.append(dictSemigroup)(v.value0)($242.value0), $242.value1);
        };
    });
};
var applicativeTuple = function (dictMonoid) {
    return new Control_Applicative.Applicative(function () {
        return applyTuple(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]());
    }, Tuple.create(Data_Monoid.mempty(dictMonoid)));
};
var monadTuple = function (dictMonoid) {
    return new Control_Monad.Monad(function () {
        return applicativeTuple(dictMonoid);
    }, function () {
        return bindTuple(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]());
    });
};
module.exports = {
    Tuple: Tuple, 
    curry: curry, 
    fst: fst, 
    lookup: lookup, 
    snd: snd, 
    swap: swap, 
    uncurry: uncurry, 
    showTuple: showTuple, 
    eqTuple: eqTuple, 
    ordTuple: ordTuple, 
    boundedTuple: boundedTuple, 
    semigroupoidTuple: semigroupoidTuple, 
    semigroupTuple: semigroupTuple, 
    monoidTuple: monoidTuple, 
    semiringTuple: semiringTuple, 
    ringTuple: ringTuple, 
    commutativeRingTuple: commutativeRingTuple, 
    heytingAlgebraTuple: heytingAlgebraTuple, 
    booleanAlgebraTuple: booleanAlgebraTuple, 
    functorTuple: functorTuple, 
    invariantTuple: invariantTuple, 
    bifunctorTuple: bifunctorTuple, 
    applyTuple: applyTuple, 
    biapplyTuple: biapplyTuple, 
    applicativeTuple: applicativeTuple, 
    biapplicativeTuple: biapplicativeTuple, 
    bindTuple: bindTuple, 
    monadTuple: monadTuple, 
    extendTuple: extendTuple, 
    comonadTuple: comonadTuple, 
    lazyTuple: lazyTuple, 
    foldableTuple: foldableTuple, 
    bifoldableTuple: bifoldableTuple, 
    traversableTuple: traversableTuple, 
    bitraversableTuple: bitraversableTuple
};

},{"../Control.Applicative":7,"../Control.Apply":9,"../Control.Biapplicative":10,"../Control.Biapply":11,"../Control.Bind":13,"../Control.Comonad":15,"../Control.Extend":16,"../Control.Lazy":17,"../Control.Monad":48,"../Control.Semigroupoid":53,"../Data.Bifoldable":68,"../Data.Bifunctor":69,"../Data.Bitraversable":70,"../Data.BooleanAlgebra":72,"../Data.Bounded":74,"../Data.CommutativeRing":75,"../Data.Eq":79,"../Data.Foldable":84,"../Data.Function":97,"../Data.Functor":100,"../Data.Functor.Invariant":98,"../Data.HeytingAlgebra":104,"../Data.Maybe":113,"../Data.Maybe.First":111,"../Data.Monoid":120,"../Data.Ord":127,"../Data.Ordering":128,"../Data.Ring":130,"../Data.Semigroup":132,"../Data.Semiring":134,"../Data.Show":136,"../Data.Traversable":146,"../Data.Unit":151}],148:[function(require,module,exports){
"use strict";

// module Data.Unfoldable

exports.unfoldrArrayImpl = function (isNothing) {
  return function (fromJust) {
    return function (fst) {
      return function (snd) {
        return function (f) {
          return function (b) {
            var result = [];
            while (true) {
              var maybe = f(b);
              if (isNothing(maybe)) return result;
              var tuple = fromJust(maybe);
              result.push(fst(tuple));
              b = snd(tuple);
            }
          };
        };
      };
    };
  };
};

},{}],149:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Maybe = require("../Data.Maybe");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Partial_Unsafe = require("../Partial.Unsafe");
var Data_Ord = require("../Data.Ord");
var Data_Ring = require("../Data.Ring");
var Data_Function = require("../Data.Function");
var Data_Unit = require("../Data.Unit");
var Data_Functor = require("../Data.Functor");
var Unfoldable = function (unfoldr) {
    this.unfoldr = unfoldr;
};
var unfoldr = function (dict) {
    return dict.unfoldr;
};
var unfoldableArray = new Unfoldable($foreign.unfoldrArrayImpl(Data_Maybe.isNothing)(Partial_Unsafe.unsafePartial(function (dictPartial) {
    return Data_Maybe.fromJust(dictPartial);
}))(Data_Tuple.fst)(Data_Tuple.snd));
var replicate = function (dictUnfoldable) {
    return function (n) {
        return function (v) {
            var step = function (i) {
                var $8 = i <= 0;
                if ($8) {
                    return Data_Maybe.Nothing.value;
                };
                if (!$8) {
                    return new Data_Maybe.Just(new Data_Tuple.Tuple(v, i - 1));
                };
                throw new Error("Failed pattern match at Data.Unfoldable line 59, column 7 - line 60, column 34: " + [ $8.constructor.name ]);
            };
            return unfoldr(dictUnfoldable)(step)(n);
        };
    };
};
var replicateA = function (dictApplicative) {
    return function (dictUnfoldable) {
        return function (dictTraversable) {
            return function (n) {
                return function (m) {
                    return Data_Traversable.sequence(dictTraversable)(dictApplicative)(replicate(dictUnfoldable)(n)(m));
                };
            };
        };
    };
};
var singleton = function (dictUnfoldable) {
    return replicate(dictUnfoldable)(1);
};
var none = function (dictUnfoldable) {
    return unfoldr(dictUnfoldable)(Data_Function["const"](Data_Maybe.Nothing.value))(Data_Unit.unit);
};
var fromMaybe = function (dictUnfoldable) {
    return unfoldr(dictUnfoldable)(function (b) {
        return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Function.flip(Data_Tuple.Tuple.create)(Data_Maybe.Nothing.value))(b);
    });
};
module.exports = {
    Unfoldable: Unfoldable, 
    fromMaybe: fromMaybe, 
    none: none, 
    replicate: replicate, 
    replicateA: replicateA, 
    singleton: singleton, 
    unfoldr: unfoldr, 
    unfoldableArray: unfoldableArray
};

},{"../Data.Function":97,"../Data.Functor":100,"../Data.Maybe":113,"../Data.Ord":127,"../Data.Ring":130,"../Data.Traversable":146,"../Data.Tuple":147,"../Data.Unit":151,"../Partial.Unsafe":159,"../Prelude":162,"./foreign":148}],150:[function(require,module,exports){
"use strict";

// module Data.Unit

exports.unit = {};

},{}],151:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Data_Show = require("../Data.Show");
var showUnit = new Data_Show.Show(function (v) {
    return "unit";
});
module.exports = {
    showUnit: showUnit, 
    unit: $foreign.unit
};

},{"../Data.Show":136,"./foreign":150}],152:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Data_Show = require("../Data.Show");
var Void = function (x) {
    return x;
};
var absurd = function (a) {
    var spin = function (__copy_v) {
        var v = __copy_v;
        tco: while (true) {
            var __tco_v = v;
            v = __tco_v;
            continue tco;
        };
    };
    return spin(a);
};
var showVoid = new Data_Show.Show(absurd);
module.exports = {
    absurd: absurd, 
    showVoid: showVoid
};

},{"../Data.Show":136}],153:[function(require,module,exports){
// module Inferno

var Inferno = require("inferno");
var InfernoDOM = require("inferno-dom");

exports._staticVElement = Inferno.createStaticVElement;
exports.createOptBlueprint = Inferno.createOptBlueprint;
exports.prop = function(str){
  return function(a){
    return [str, a];
  };
};
exports.props = function(props){
  var propObj = {};
  for (var i = 0; i < props.length; i++){
    propObj[props[i][0]] = props[i][1];
  }
  return propObj;
};
exports.render = function(inode){
  return function(elem){
    return function(){
      InfernoDOM.render(inode, elem);
    };
  };
};

},{"inferno":4,"inferno-dom":1}],154:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var DOM = require("../DOM");
var DOM_Node_Types = require("../DOM.Node.Types");
var Data_Function_Uncurried = require("../Data.Function.Uncurried");
var Data_Maybe = require("../Data.Maybe");
var Data_Nullable = require("../Data.Nullable");
var Data_StrMap = require("../Data.StrMap");
var Prelude = require("../Prelude");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var Data_Function = require("../Data.Function");
var NoDynamicValue = (function () {
    function NoDynamicValue() {

    };
    NoDynamicValue.value = new NoDynamicValue();
    return NoDynamicValue;
})();
var DynamicValueInt = (function () {
    function DynamicValueInt(value0) {
        this.value0 = value0;
    };
    DynamicValueInt.create = function (value0) {
        return new DynamicValueInt(value0);
    };
    return DynamicValueInt;
})();
var DynamicValueString = (function () {
    function DynamicValueString(value0) {
        this.value0 = value0;
    };
    DynamicValueString.create = function (value0) {
        return new DynamicValueString(value0);
    };
    return DynamicValueString;
})();
var Dynamic = (function () {
    function Dynamic(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Dynamic.create = function (value0) {
        return function (value1) {
            return new Dynamic(value0, value1);
        };
    };
    return Dynamic;
})();
var ChildrenType = (function () {
    function ChildrenType(value0) {
        this.value0 = value0;
    };
    ChildrenType.create = function (value0) {
        return new ChildrenType(value0);
    };
    return ChildrenType;
})();
var toOptElement = Unsafe_Coerce.unsafeCoerce;
var toBP4 = Unsafe_Coerce.unsafeCoerce;
var toBP3 = Unsafe_Coerce.unsafeCoerce;
var toBP2 = Unsafe_Coerce.unsafeCoerce;
var toBP1 = Unsafe_Coerce.unsafeCoerce;
var toBP0 = Unsafe_Coerce.unsafeCoerce;
var text = new ChildrenType(4);
var staticVElement = Data_Function_Uncurried.runFn3($foreign._staticVElement);
var $$null = Data_Nullable.toNullable(Data_Maybe.Nothing.value);
var runBP0 = function (bp) {
    return toOptElement({
        bp: bp, 
        dom: $$null, 
        type: 2, 
        v0: $$null, 
        v1: $$null, 
        v2: $$null, 
        v3: $$null
    });
};
var runBP1 = function (bp) {
    return function (a) {
        return toOptElement({
            bp: bp, 
            dom: $$null, 
            type: 2, 
            v0: a, 
            v1: $$null, 
            v2: $$null, 
            v3: $$null
        });
    };
};
var runBP2 = function (bp) {
    return function (a) {
        return function (b) {
            return toOptElement({
                bp: bp, 
                dom: $$null, 
                type: 2, 
                v0: a, 
                v1: b, 
                v2: $$null, 
                v3: $$null
            });
        };
    };
};
var runBP3 = function (bp) {
    return function (a) {
        return function (b) {
            return function (c) {
                return toOptElement({
                    bp: bp, 
                    dom: $$null, 
                    type: 2, 
                    v0: a, 
                    v1: b, 
                    v2: c, 
                    v3: $$null
                });
            };
        };
    };
};
var runBP4 = function (bp) {
    return function (a) {
        return function (b) {
            return function (c) {
                return function (d) {
                    return toOptElement({
                        bp: bp, 
                        dom: $$null, 
                        type: 2, 
                        v0: a, 
                        v1: b, 
                        v2: c, 
                        v3: d
                    });
                };
            };
        };
    };
};
var toFragment = function (v) {
    return function (children) {
        return Unsafe_Coerce.unsafeCoerce({
            dom: $$null, 
            pointer: $$null, 
            type: 4, 
            children: children, 
            childrenType: v.value0
        });
    };
};
var toSecondInfernoPart = function (v) {
    if (v instanceof NoDynamicValue) {
        return $$null;
    };
    if (v instanceof DynamicValueInt) {
        return Unsafe_Coerce.unsafeCoerce(v.value0);
    };
    if (v instanceof DynamicValueString) {
        return Unsafe_Coerce.unsafeCoerce(v.value0);
    };
    throw new Error("Failed pattern match at Inferno line 174, column 1 - line 174, column 42: " + [ v.constructor.name ]);
};
var toTextNode = function (str) {
    return Unsafe_Coerce.unsafeCoerce({
        dom: $$null, 
        type: 3, 
        text: str
    });
};
var nonKeyed = new ChildrenType(1);
var node = new ChildrenType(3);
var mkDynamic = function (i) {
    return function (j) {
        return new Dynamic(i, j);
    };
};
var propD = function (propName) {
    return mkDynamic(8)(new DynamicValueString(propName));
};
var refD = mkDynamic(5)(NoDynamicValue.value);
var spreadD = mkDynamic(6)(NoDynamicValue.value);
var styleD = mkDynamic(3)(NoDynamicValue.value);
var valueD = mkDynamic(7)(NoDynamicValue.value);
var makeBP4 = function ($$static) {
    return function (v) {
        return function (v1) {
            return function (v2) {
                return function (v3) {
                    return Data_Function.apply(toBP4)($foreign.createOptBlueprint($$static, Data_Function.apply(Data_Nullable.toNullable)(new Data_Maybe.Just(v.value0)), toSecondInfernoPart(v.value1), Data_Function.apply(Data_Nullable.toNullable)(new Data_Maybe.Just(v1.value0)), toSecondInfernoPart(v1.value1), Data_Function.apply(Data_Nullable.toNullable)(new Data_Maybe.Just(v2.value0)), toSecondInfernoPart(v2.value1), Data_Function.apply(Data_Nullable.toNullable)(new Data_Maybe.Just(v3.value0)), toSecondInfernoPart(v3.value1)));
                };
            };
        };
    };
};
var makeBP3 = function ($$static) {
    return function (v) {
        return function (v1) {
            return function (v2) {
                return Data_Function.apply(toBP3)($foreign.createOptBlueprint($$static, Data_Function.apply(Data_Nullable.toNullable)(new Data_Maybe.Just(v.value0)), toSecondInfernoPart(v.value1), Data_Function.apply(Data_Nullable.toNullable)(new Data_Maybe.Just(v1.value0)), toSecondInfernoPart(v1.value1), Data_Function.apply(Data_Nullable.toNullable)(new Data_Maybe.Just(v2.value0)), toSecondInfernoPart(v2.value1), $$null, $$null));
            };
        };
    };
};
var makeBP2 = function ($$static) {
    return function (v) {
        return function (v1) {
            return Data_Function.apply(toBP2)($foreign.createOptBlueprint($$static, Data_Function.apply(Data_Nullable.toNullable)(new Data_Maybe.Just(v.value0)), toSecondInfernoPart(v.value1), Data_Function.apply(Data_Nullable.toNullable)(new Data_Maybe.Just(v1.value0)), toSecondInfernoPart(v1.value1), $$null, $$null, $$null, $$null));
        };
    };
};
var makeBP1 = function ($$static) {
    return function (v) {
        return Data_Function.apply(toBP1)($foreign.createOptBlueprint($$static, Data_Function.apply(Data_Nullable.toNullable)(new Data_Maybe.Just(v.value0)), toSecondInfernoPart(v.value1), $$null, $$null, $$null, $$null, $$null, $$null));
    };
};
var makeBP0 = function ($$static) {
    return Data_Function.apply(toBP0)($foreign.createOptBlueprint($$static, $$null, $$null, $$null, $$null, $$null, $$null, $$null, $$null));
};
var keyed = new ChildrenType(2);
var dataD = mkDynamic(4)(NoDynamicValue.value);
var classNameD = mkDynamic(2)(NoDynamicValue.value);
var childrenD = function (v) {
    return mkDynamic(1)(new DynamicValueInt(v.value0));
};
module.exports = {
    ChildrenType: ChildrenType, 
    Dynamic: Dynamic, 
    NoDynamicValue: NoDynamicValue, 
    DynamicValueInt: DynamicValueInt, 
    DynamicValueString: DynamicValueString, 
    childrenD: childrenD, 
    classNameD: classNameD, 
    dataD: dataD, 
    keyed: keyed, 
    makeBP0: makeBP0, 
    makeBP1: makeBP1, 
    makeBP2: makeBP2, 
    makeBP3: makeBP3, 
    makeBP4: makeBP4, 
    mkDynamic: mkDynamic, 
    node: node, 
    nonKeyed: nonKeyed, 
    "null": $$null, 
    propD: propD, 
    refD: refD, 
    runBP0: runBP0, 
    runBP1: runBP1, 
    runBP2: runBP2, 
    runBP3: runBP3, 
    runBP4: runBP4, 
    spreadD: spreadD, 
    staticVElement: staticVElement, 
    styleD: styleD, 
    text: text, 
    toBP0: toBP0, 
    toBP1: toBP1, 
    toBP2: toBP2, 
    toBP3: toBP3, 
    toBP4: toBP4, 
    toFragment: toFragment, 
    toOptElement: toOptElement, 
    toSecondInfernoPart: toSecondInfernoPart, 
    toTextNode: toTextNode, 
    valueD: valueD, 
    _staticVElement: $foreign._staticVElement, 
    createOptBlueprint: $foreign.createOptBlueprint, 
    prop: $foreign.prop, 
    props: $foreign.props, 
    render: $foreign.render
};

},{"../Control.Monad.Eff":34,"../DOM":65,"../DOM.Node.Types":64,"../Data.Function":97,"../Data.Function.Uncurried":96,"../Data.Maybe":113,"../Data.Nullable":123,"../Data.StrMap":140,"../Prelude":162,"../Unsafe.Coerce":165,"./foreign":153}],155:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Eff_Console = require("../Control.Monad.Eff.Console");
var DOM_HTML = require("../DOM.HTML");
var DOM_HTML_Types = require("../DOM.HTML.Types");
var DOM_HTML_Window = require("../DOM.HTML.Window");
var DOM_Node_NonElementParentNode = require("../DOM.Node.NonElementParentNode");
var DOM_Node_Types = require("../DOM.Node.Types");
var Data_Maybe = require("../Data.Maybe");
var Data_Nullable = require("../Data.Nullable");
var Inferno = require("../Inferno");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Data_Function = require("../Data.Function");
var helloDiv = Inferno.makeBP1(Inferno.staticVElement("div")(Inferno.props([ Inferno.prop("class")("hey") ]))([  ]))(Inferno.childrenD(Inferno.node));
var go = function (elem) {
    return function (str) {
        return Data_Maybe.maybe(Control_Monad_Eff_Console.log("Couldn't find app elem"))(Inferno.render(Inferno.runBP1(helloDiv)(Inferno.toTextNode(str))))(Data_Nullable.toMaybe(elem));
    };
};
var main = function __do() {
    var v = DOM_HTML.window();
    var v1 = DOM_HTML_Window.document(v)();
    var dn = DOM_HTML_Types.htmlDocumentToNonElementParentNode(v1);
    var v2 = DOM_Node_NonElementParentNode.getElementById("app")(dn)();
    go(v2)("hey first")();
    Data_Function.apply(Control_Monad_Aff.launchAff)(Control_Monad_Aff["later'"](2000)(Data_Function.apply(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff))(go(v2)("hey after 2 seconds"))))();
    return Control_Monad_Eff_Console.log("Hello sailor!")();
};
module.exports = {
    go: go, 
    helloDiv: helloDiv, 
    main: main
};

},{"../Control.Bind":13,"../Control.Monad.Aff":21,"../Control.Monad.Eff":34,"../Control.Monad.Eff.Class":24,"../Control.Monad.Eff.Console":26,"../DOM.HTML":61,"../DOM.HTML.Types":57,"../DOM.HTML.Window":59,"../DOM.Node.NonElementParentNode":63,"../DOM.Node.Types":64,"../Data.Function":97,"../Data.Maybe":113,"../Data.Nullable":123,"../Inferno":154,"../Prelude":162}],156:[function(require,module,exports){
"use strict";

// module Math

exports.abs = Math.abs;

exports.acos = Math.acos;

exports.asin = Math.asin;

exports.atan = Math.atan;

exports.atan2 = function (y) {
  return function (x) {
    return Math.atan2(y, x);
  };
};

exports.ceil = Math.ceil;

exports.cos = Math.cos;

exports.exp = Math.exp;

exports.floor = Math.floor;

exports.trunc = Math.trunc || function (n) {
  return n < 0 ? Math.ceil(n) : Math.floor(n);
};

exports.log = Math.log;

exports.max = function (n1) {
  return function (n2) {
    return Math.max(n1, n2);
  };
};

exports.min = function (n1) {
  return function (n2) {
    return Math.min(n1, n2);
  };
};

exports.pow = function (n) {
  return function (p) {
    return Math.pow(n, p);
  };
};

exports.remainder = function (n) {
  return function (m) {
    return n % m;
  };
};

exports.round = Math.round;

exports.sin = Math.sin;

exports.sqrt = Math.sqrt;

exports.tan = Math.tan;

exports.e = Math.E;

exports.ln2 = Math.LN2;

exports.ln10 = Math.LN10;

exports.log2e = Math.LOG2E;

exports.log10e = Math.LOG10E;

exports.pi = Math.PI;

exports.sqrt1_2 = Math.SQRT1_2;

exports.sqrt2 = Math.SQRT2;

},{}],157:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
module.exports = {
    abs: $foreign.abs, 
    acos: $foreign.acos, 
    asin: $foreign.asin, 
    atan: $foreign.atan, 
    atan2: $foreign.atan2, 
    ceil: $foreign.ceil, 
    cos: $foreign.cos, 
    e: $foreign.e, 
    exp: $foreign.exp, 
    floor: $foreign.floor, 
    ln10: $foreign.ln10, 
    ln2: $foreign.ln2, 
    log: $foreign.log, 
    log10e: $foreign.log10e, 
    log2e: $foreign.log2e, 
    max: $foreign.max, 
    min: $foreign.min, 
    pi: $foreign.pi, 
    pow: $foreign.pow, 
    remainder: $foreign.remainder, 
    round: $foreign.round, 
    sin: $foreign.sin, 
    sqrt: $foreign.sqrt, 
    sqrt1_2: $foreign.sqrt1_2, 
    sqrt2: $foreign.sqrt2, 
    tan: $foreign.tan, 
    trunc: $foreign.trunc
};

},{"./foreign":156}],158:[function(require,module,exports){
"use strict";

// module Partial.Unsafe

exports.unsafePartial = function (f) {
  return f();
};

},{}],159:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Partial = require("../Partial");
var unsafeCrashWith = function (msg) {
    return $foreign.unsafePartial(function (dictPartial) {
        return Partial.crashWith(dictPartial)(msg);
    });
};
module.exports = {
    unsafeCrashWith: unsafeCrashWith, 
    unsafePartial: $foreign.unsafePartial
};

},{"../Partial":161,"./foreign":158}],160:[function(require,module,exports){
"use strict";

// module Partial

exports.crashWith = function () {
  return function (msg) {
    throw new Error(msg);
  };
};

},{}],161:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var crash = function (dictPartial) {
    return $foreign.crashWith(dictPartial)("Partial.crash: partial function");
};
module.exports = {
    crash: crash, 
    crashWith: $foreign.crashWith
};

},{"./foreign":160}],162:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Category = require("../Control.Category");
var Control_Monad = require("../Control.Monad");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Boolean = require("../Data.Boolean");
var Data_BooleanAlgebra = require("../Data.BooleanAlgebra");
var Data_Bounded = require("../Data.Bounded");
var Data_CommutativeRing = require("../Data.CommutativeRing");
var Data_Eq = require("../Data.Eq");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Field = require("../Data.Field");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_NaturalTransformation = require("../Data.NaturalTransformation");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_Unit = require("../Data.Unit");
var Data_Void = require("../Data.Void");
module.exports = {};

},{"../Control.Applicative":7,"../Control.Apply":9,"../Control.Bind":13,"../Control.Category":14,"../Control.Monad":48,"../Control.Semigroupoid":53,"../Data.Boolean":71,"../Data.BooleanAlgebra":72,"../Data.Bounded":74,"../Data.CommutativeRing":75,"../Data.Eq":79,"../Data.EuclideanRing":81,"../Data.Field":82,"../Data.Function":97,"../Data.Functor":100,"../Data.HeytingAlgebra":104,"../Data.NaturalTransformation":121,"../Data.Ord":127,"../Data.Ordering":128,"../Data.Ring":130,"../Data.Semigroup":132,"../Data.Semiring":134,"../Data.Show":136,"../Data.Unit":151,"../Data.Void":152}],163:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var Proxy3 = (function () {
    function Proxy3() {

    };
    Proxy3.value = new Proxy3();
    return Proxy3;
})();
var Proxy2 = (function () {
    function Proxy2() {

    };
    Proxy2.value = new Proxy2();
    return Proxy2;
})();
var $$Proxy = (function () {
    function Proxy() {

    };
    Proxy.value = new Proxy();
    return Proxy;
})();
module.exports = {
    "Proxy": $$Proxy, 
    Proxy2: Proxy2, 
    Proxy3: Proxy3
};

},{}],164:[function(require,module,exports){
"use strict";

// module Unsafe.Coerce

exports.unsafeCoerce = function (x) {
  return x;
};

},{}],165:[function(require,module,exports){
// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
module.exports = {
    unsafeCoerce: $foreign.unsafeCoerce
};

},{"./foreign":164}],166:[function(require,module,exports){
require('Main').main();

},{"Main":155}]},{},[166]);
