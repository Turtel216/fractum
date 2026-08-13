/**
 * TypedJS Runtime — Virtual DOM Engine & Elm Architecture Bootstrapper
 *
 * This is the core JavaScript runtime that powers the TypedJS browser
 * framework. It consumes compiled ADT objects (using the { _tag, _0, _1, ... }
 * convention) and manages the Model-View-Update loop.
 *
 * Zero external dependencies. Pure vanilla JavaScript.
 *
 * ADT Encoding Convention (from the TypedJS compiler):
 *   Variant with args:    { _tag: "Name", _0: arg0, _1: arg1, ... }
 *   Variant without args: { _tag: "Name" }
 *   Lists/Arrays:         Native JS arrays [...]
 *   Strings:              Native JS strings
 *   Functions:            Direct JS function calls f(value)
 *   Booleans:             Native JS booleans
 *   Numbers:              Native JS numbers
 */
(function (global) {
  'use strict';

  // ============= ADT HELPER CONSTRUCTORS =============
  // These mirror the compiler's output for Option and Result types,
  // used by the runtime to wrap JS values before passing them back
  // into the typed update loop.

  const _Some = (v) => ({ _tag: 'Some', _0: v });
  const _None = Object.freeze({ _tag: 'None' });
  const _Ok = (v) => ({ _tag: 'Ok', _0: v });
  const _Err = (v) => ({ _tag: 'Err', _0: v });

  // ============= PATCH TYPE CONSTANTS =============

  const PATCH_REPLACE  = 1;
  const PATCH_TEXT     = 2;
  const PATCH_UPDATE   = 3;  // attrs + children changed on a Node/Keyed
  const PATCH_INSERT   = 4;
  const PATCH_REMOVE   = 5;

  // ============= EVENT SANITIZATION BOUNDARY =============
  //
  // This is the critical firewall between the mutable browser world and the
  // pure TypedJS update loop. Raw DOM Event objects NEVER cross this boundary.
  // Each event variant extracts ONLY the typed primitive data specified by the
  // Attribute<Msg> ADT, then calls the user's mapper function or dispatches
  // the user's Msg directly.
  //
  // OnClick(msg)           → dispatches msg (no event data extracted)
  // OnInput(f)             → extracts e.target.value as String, dispatches f(value)
  // OnSubmit(msg)          → prevents default, dispatches msg
  // OnChange(f)            → extracts e.target.value as String, dispatches f(value)
  // OnMouseEnter(msg)      → dispatches msg
  // OnMouseLeave(msg)      → dispatches msg
  // OnKeyDown(f)           → extracts e.keyCode as Int, dispatches f(keyCode)
  // OnFocus(msg)           → dispatches msg
  // OnBlur(msg)            → dispatches msg

  const EVENT_MAP = {
    'OnClick':      'click',
    'OnInput':      'input',
    'OnSubmit':     'submit',
    'OnChange':     'change',
    'OnMouseEnter': 'mouseenter',
    'OnMouseLeave': 'mouseleave',
    'OnKeyDown':    'keydown',
    'OnFocus':      'focus',
    'OnBlur':       'blur',
  };

  function createEventHandler(attr, dispatch) {
    switch (attr._tag) {
      case 'OnClick':
        return () => dispatch(attr._0);
      case 'OnInput':
        return (e) => dispatch(attr._0(e.target.value));
      case 'OnSubmit':
        return (e) => { e.preventDefault(); dispatch(attr._0); };
      case 'OnChange':
        return (e) => dispatch(attr._0(e.target.value));
      case 'OnMouseEnter':
        return () => dispatch(attr._0);
      case 'OnMouseLeave':
        return () => dispatch(attr._0);
      case 'OnKeyDown':
        return (e) => dispatch(attr._0(e.keyCode));
      case 'OnFocus':
        return () => dispatch(attr._0);
      case 'OnBlur':
        return () => dispatch(attr._0);
      default:
        return null;
    }
  }

  // ============= VIRTUAL DOM RENDERER =============

  /**
   * Apply a single Attribute<Msg> ADT to a real DOM element.
   * Event listeners are stored on the element for later cleanup during patching.
   */
  function applyAttribute(el, attr, dispatch) {
    const eventName = EVENT_MAP[attr._tag];
    if (eventName) {
      // Event handler — goes through the sanitization boundary
      const handler = createEventHandler(attr, dispatch);
      if (handler) {
        bindEvent(el, eventName, handler);
      }
      return;
    }

    switch (attr._tag) {
      case 'Class':
        // Append to existing classes rather than overwrite, so multiple
        // Class attributes compose correctly.
        if (el.className) {
          el.className += ' ' + attr._0;
        } else {
          el.className = attr._0;
        }
        break;
      case 'Id':
        el.id = attr._0;
        break;
      case 'Style':
        el.style[attr._0] = attr._1;
        break;
      case 'Value':
        // Use property assignment for input value (not setAttribute)
        el.value = attr._0;
        break;
      case 'Disabled':
        el.disabled = !!attr._0;
        break;
      case 'Placeholder':
        el.setAttribute('placeholder', attr._0);
        break;
      case 'Type':
        el.setAttribute('type', attr._0);
        break;
      case 'Href':
        el.setAttribute('href', attr._0);
        break;
      case 'Src':
        el.setAttribute('src', attr._0);
        break;
      case 'Alt':
        el.setAttribute('alt', attr._0);
        break;
    }
  }

  /**
   * Bind an event listener to a DOM element, replacing any previous listener
   * for the same event type. Stores reference for later cleanup.
   */
  function bindEvent(el, eventName, handler) {
    if (!el.__tjs_events) el.__tjs_events = {};
    if (el.__tjs_events[eventName]) {
      el.removeEventListener(eventName, el.__tjs_events[eventName]);
    }
    el.__tjs_events[eventName] = handler;
    el.addEventListener(eventName, handler);
  }

  /**
   * Remove all TypedJS-managed event listeners from a DOM element.
   */
  function removeAllEvents(el) {
    if (!el.__tjs_events) return;
    for (const eventName in el.__tjs_events) {
      el.removeEventListener(eventName, el.__tjs_events[eventName]);
    }
    el.__tjs_events = {};
  }

  /**
   * Create a real DOM node from a compiled Html<Msg> ADT object.
   *
   * @param {object} vnode - The compiled Html<Msg> ADT
   * @param {function} dispatch - The message dispatch function
   * @returns {Node} A real DOM node
   */
  function createElement(vnode, dispatch) {
    if (!vnode) return document.createComment('empty');

    switch (vnode._tag) {
      case 'Node': {
        const el = document.createElement(vnode._0);
        const attrs = vnode._1;
        const children = vnode._2;

        for (let i = 0; i < attrs.length; i++) {
          applyAttribute(el, attrs[i], dispatch);
        }
        for (let i = 0; i < children.length; i++) {
          el.appendChild(createElement(children[i], dispatch));
        }
        return el;
      }

      case 'Keyed': {
        const el = document.createElement(vnode._0);
        const attrs = vnode._1;
        const keyedItems = vnode._2;

        for (let i = 0; i < attrs.length; i++) {
          applyAttribute(el, attrs[i], dispatch);
        }
        for (let i = 0; i < keyedItems.length; i++) {
          // KeyedItem ADT: { _tag: "KeyedItem", _0: key, _1: childVNode }
          const item = keyedItems[i];
          const childEl = createElement(item._1, dispatch);
          childEl.__tjs_key = item._0;
          el.appendChild(childEl);
        }
        return el;
      }

      case 'Text':
        return document.createTextNode(vnode._0);

      case 'Empty':
        return document.createComment('empty');

      default:
        return document.createComment('unknown');
    }
  }

  // ============= DIFFING ENGINE =============

  /**
   * Compute a unique key for an attribute, distinguishing Style properties.
   */
  function attrKey(attr) {
    return attr._tag === 'Style' ? 'Style:' + attr._0 : attr._tag;
  }

  /**
   * Check if an attribute tag represents an event handler.
   */
  function isEventAttr(tag) {
    return tag in EVENT_MAP;
  }

  /**
   * Diff two attribute arrays. Returns null if identical.
   */
  function diffAttrs(oldAttrs, newAttrs) {
    const toSet = [];
    const toRemove = [];

    const oldMap = new Map();
    for (let i = 0; i < oldAttrs.length; i++) {
      oldMap.set(attrKey(oldAttrs[i]), oldAttrs[i]);
    }

    const newMap = new Map();
    for (let i = 0; i < newAttrs.length; i++) {
      const key = attrKey(newAttrs[i]);
      newMap.set(key, newAttrs[i]);
      const oldAttr = oldMap.get(key);

      if (!oldAttr) {
        // New attribute added
        toSet.push(newAttrs[i]);
      } else if (isEventAttr(newAttrs[i]._tag)) {
        // Event handlers: always re-bind to capture fresh closures/Msg values
        toSet.push(newAttrs[i]);
      } else {
        // Compare primitive attribute values
        if (!shallowEqualAttr(oldAttr, newAttrs[i])) {
          toSet.push(newAttrs[i]);
        }
      }
    }

    // Detect removed attributes
    oldMap.forEach((oldAttr, key) => {
      if (!newMap.has(key)) {
        toRemove.push(oldAttr);
      }
    });

    if (toSet.length === 0 && toRemove.length === 0) return null;
    return { set: toSet, remove: toRemove };
  }

  /**
   * Shallow equality check for non-event attributes.
   */
  function shallowEqualAttr(a, b) {
    if (a._tag !== b._tag) return false;
    if (a._0 !== b._0) return false;
    if (a._1 !== undefined && a._1 !== b._1) return false;
    return true;
  }

  /**
   * Diff two Html<Msg> VNode trees. Returns a patch object, or null if identical.
   */
  function diff(oldV, newV) {
    // Insertions and removals
    if (oldV === undefined || oldV === null) {
      return { type: PATCH_INSERT, vnode: newV };
    }
    if (newV === undefined || newV === null) {
      return { type: PATCH_REMOVE };
    }

    // Different top-level variant → full replace
    if (oldV._tag !== newV._tag) {
      return { type: PATCH_REPLACE, vnode: newV };
    }

    switch (oldV._tag) {
      case 'Text':
        return oldV._0 === newV._0
          ? null
          : { type: PATCH_TEXT, text: newV._0 };

      case 'Empty':
        return null;

      case 'Node': {
        // Different element tag (e.g., "div" → "span") → replace
        if (oldV._0 !== newV._0) {
          return { type: PATCH_REPLACE, vnode: newV };
        }
        const ap = diffAttrs(oldV._1, newV._1);
        const cp = diffUnkeyedChildren(oldV._2, newV._2);
        return (ap || cp)
          ? { type: PATCH_UPDATE, attrs: ap, children: cp, keyed: false }
          : null;
      }

      case 'Keyed': {
        if (oldV._0 !== newV._0) {
          return { type: PATCH_REPLACE, vnode: newV };
        }
        const ap = diffAttrs(oldV._1, newV._1);
        const cp = diffKeyedChildren(oldV._2, newV._2);
        return (ap || cp)
          ? { type: PATCH_UPDATE, attrs: ap, children: cp, keyed: true }
          : null;
      }
    }

    return null;
  }

  /**
   * Diff unkeyed children arrays. Returns an array of per-index patches, or null.
   */
  function diffUnkeyedChildren(oldKids, newKids) {
    const len = Math.max(oldKids.length, newKids.length);
    const patches = new Array(len);
    let hasChanges = false;

    for (let i = 0; i < len; i++) {
      const p = diff(oldKids[i], newKids[i]);
      patches[i] = p;
      if (p !== null) hasChanges = true;
    }

    return hasChanges ? patches : null;
  }

  /**
   * Diff keyed children arrays. Uses a Map for O(n) key lookups.
   *
   * Returns an object describing: { inserts, removes, updates, order }
   * where 'order' is the final key sequence for reordering.
   */
  function diffKeyedChildren(oldItems, newItems) {
    // Build key → { index, vnode } maps
    const oldMap = new Map();
    for (let i = 0; i < oldItems.length; i++) {
      oldMap.set(oldItems[i]._0, { idx: i, vnode: oldItems[i]._1 });
    }

    const inserts = [];   // { key, vnode, atIndex }
    const removes = [];   // keys to remove
    const updates = {};   // key → patch
    const order = [];     // final key ordering
    let hasChanges = false;

    const seenKeys = new Set();

    for (let i = 0; i < newItems.length; i++) {
      const key = newItems[i]._0;
      const newChild = newItems[i]._1;
      order.push(key);
      seenKeys.add(key);

      const oldEntry = oldMap.get(key);
      if (oldEntry) {
        const p = diff(oldEntry.vnode, newChild);
        if (p) {
          updates[key] = p;
          hasChanges = true;
        }
      } else {
        inserts.push({ key, vnode: newChild, atIndex: i });
        hasChanges = true;
      }
    }

    // Detect removed keys
    for (let i = 0; i < oldItems.length; i++) {
      if (!seenKeys.has(oldItems[i]._0)) {
        removes.push(oldItems[i]._0);
        hasChanges = true;
      }
    }

    // Check if order changed (even if individual children didn't)
    if (!hasChanges) {
      for (let i = 0; i < oldItems.length; i++) {
        if (i >= newItems.length || oldItems[i]._0 !== newItems[i]._0) {
          hasChanges = true;
          break;
        }
      }
    }

    return hasChanges
      ? { inserts, removes, updates, order }
      : null;
  }

  // ============= PATCHING =============

  /**
   * Apply a patch to a real DOM node. Returns the (possibly replaced) DOM node.
   */
  function applyPatches(domNode, patch, dispatch) {
    if (!patch) return domNode;

    switch (patch.type) {
      case PATCH_REPLACE: {
        const newEl = createElement(patch.vnode, dispatch);
        if (domNode && domNode.parentNode) {
          domNode.parentNode.replaceChild(newEl, domNode);
        }
        return newEl;
      }

      case PATCH_TEXT: {
        domNode.textContent = patch.text;
        return domNode;
      }

      case PATCH_INSERT: {
        return createElement(patch.vnode, dispatch);
      }

      case PATCH_REMOVE: {
        if (domNode && domNode.parentNode) {
          removeAllEvents(domNode);
          domNode.parentNode.removeChild(domNode);
        }
        return null;
      }

      case PATCH_UPDATE: {
        // Apply attribute changes
        if (patch.attrs) {
          applyAttrPatch(domNode, patch.attrs, dispatch);
        }

        // Apply children changes
        if (patch.children) {
          if (patch.keyed) {
            applyKeyedChildPatch(domNode, patch.children, dispatch);
          } else {
            applyUnkeyedChildPatch(domNode, patch.children, dispatch);
          }
        }
        return domNode;
      }
    }

    return domNode;
  }

  /**
   * Apply attribute add/remove patch to a DOM element.
   */
  function applyAttrPatch(el, attrPatch, dispatch) {
    // Remove old attributes
    const toRemove = attrPatch.remove;
    for (let i = 0; i < toRemove.length; i++) {
      removeAttribute(el, toRemove[i]);
    }

    // Set new/updated attributes
    // First, clear className if any Class attrs are being set (to avoid accumulation)
    let hasNewClass = false;
    for (let i = 0; i < attrPatch.set.length; i++) {
      if (attrPatch.set[i]._tag === 'Class') {
        if (!hasNewClass) {
          el.className = '';
          hasNewClass = true;
        }
      }
    }

    for (let i = 0; i < attrPatch.set.length; i++) {
      applyAttribute(el, attrPatch.set[i], dispatch);
    }
  }

  /**
   * Remove a single attribute from a DOM element (inverse of applyAttribute).
   */
  function removeAttribute(el, attr) {
    const eventName = EVENT_MAP[attr._tag];
    if (eventName) {
      if (el.__tjs_events && el.__tjs_events[eventName]) {
        el.removeEventListener(eventName, el.__tjs_events[eventName]);
        delete el.__tjs_events[eventName];
      }
      return;
    }

    switch (attr._tag) {
      case 'Class':
        el.className = '';
        break;
      case 'Id':
        el.removeAttribute('id');
        break;
      case 'Style':
        el.style[attr._0] = '';
        break;
      case 'Value':
        el.value = '';
        break;
      case 'Disabled':
        el.disabled = false;
        break;
      default:
        el.removeAttribute(attr._tag.toLowerCase());
        break;
    }
  }

  /**
   * Apply unkeyed children patches (an array of per-index patches).
   */
  function applyUnkeyedChildPatch(parent, childPatches, dispatch) {
    // Snapshot childNodes before mutations
    const children = Array.from(parent.childNodes);

    for (let i = 0; i < childPatches.length; i++) {
      const p = childPatches[i];
      if (!p) continue;

      if (p.type === PATCH_INSERT) {
        const newEl = createElement(p.vnode, dispatch);
        if (i < children.length) {
          parent.insertBefore(newEl, children[i]);
        } else {
          parent.appendChild(newEl);
        }
      } else if (p.type === PATCH_REMOVE) {
        if (children[i] && children[i].parentNode === parent) {
          removeAllEvents(children[i]);
          parent.removeChild(children[i]);
        }
      } else if (children[i]) {
        applyPatches(children[i], p, dispatch);
      }
    }
  }

  /**
   * Apply keyed children patches: removals, insertions, updates, and reordering.
   */
  function applyKeyedChildPatch(parent, keyedPatch, dispatch) {
    const { inserts, removes, updates, order } = keyedPatch;

    // Build key → DOM node map from current children
    const keyMap = new Map();
    const children = Array.from(parent.childNodes);
    for (let i = 0; i < children.length; i++) {
      const key = children[i].__tjs_key;
      if (key !== undefined) {
        keyMap.set(key, children[i]);
      }
    }

    // Step 1: Remove deleted keys
    for (let i = 0; i < removes.length; i++) {
      const el = keyMap.get(removes[i]);
      if (el && el.parentNode === parent) {
        removeAllEvents(el);
        parent.removeChild(el);
        keyMap.delete(removes[i]);
      }
    }

    // Step 2: Apply in-place updates to existing nodes
    for (const key in updates) {
      const el = keyMap.get(key);
      if (el) {
        applyPatches(el, updates[key], dispatch);
      }
    }

    // Step 3: Create inserted nodes
    for (let i = 0; i < inserts.length; i++) {
      const newEl = createElement(inserts[i].vnode, dispatch);
      newEl.__tjs_key = inserts[i].key;
      keyMap.set(inserts[i].key, newEl);
    }

    // Step 4: Reorder to match the final 'order' array
    // Walk the order array and ensure each child is in the correct position.
    for (let i = 0; i < order.length; i++) {
      const el = keyMap.get(order[i]);
      if (!el) continue;

      const currentChild = parent.childNodes[i];
      if (currentChild !== el) {
        parent.insertBefore(el, currentChild || null);
      }
    }
  }

  // ============= COMMAND EXECUTOR =============

  /**
   * Interpret a Cmd<Msg> ADT object and execute the described side effect.
   * Results are always wrapped in the compiler's ADT convention (Option, Result)
   * before being dispatched back into the update loop.
   */
  function executeCmd(cmd, dispatch) {
    if (!cmd) return;

    switch (cmd._tag) {
      case 'CmdNone':
        break;

      case 'CmdBatch':
        // _0: [Cmd<Msg>]
        for (let i = 0; i < cmd._0.length; i++) {
          executeCmd(cmd._0[i], dispatch);
        }
        break;

      case 'HttpGet':
        // _0: url (String), _1: handler (Result<String, String> -> Msg)
        fetch(cmd._0)
          .then(response => {
            if (response.ok) {
              return response.text().then(body => _Ok(body));
            }
            return _Err('HTTP ' + response.status + ' ' + response.statusText);
          })
          .catch(err => _Err(String(err)))
          .then(result => dispatch(cmd._1(result)));
        break;

      case 'HttpPost':
        // _0: url, _1: body (String), _2: handler (Result<String, String> -> Msg)
        fetch(cmd._0, {
          method: 'POST',
          body: cmd._1,
          headers: { 'Content-Type': 'application/json' },
        })
          .then(response => {
            if (response.ok) {
              return response.text().then(body => _Ok(body));
            }
            return _Err('HTTP ' + response.status + ' ' + response.statusText);
          })
          .catch(err => _Err(String(err)))
          .then(result => dispatch(cmd._2(result)));
        break;

      case 'Delay':
        // _0: milliseconds (Int), _1: msg (Msg)
        setTimeout(() => dispatch(cmd._1), cmd._0);
        break;

      case 'RandomInt': {
        // _0: min (Int), _1: max (Int), _2: mapper (Int -> Msg)
        const min = cmd._0;
        const max = cmd._1;
        const value = Math.floor(Math.random() * (max - min + 1)) + min;
        dispatch(cmd._2(value));
        break;
      }

      case 'GetTime':
        // _0: mapper (Float -> Msg)
        dispatch(cmd._0(performance.now()));
        break;

      case 'FocusElement':
        // _0: element ID (String)
        // Use rAF to ensure the element exists in the DOM after the current render
        requestAnimationFrame(() => {
          const el = document.getElementById(cmd._0);
          if (el) el.focus();
        });
        break;

      case 'ConsoleLog':
        // _0: message (String)
        console.log(cmd._0);
        break;

      case 'StorageGet': {
        // _0: key (String), _1: mapper (Option<String> -> Msg)
        try {
          const val = localStorage.getItem(cmd._0);
          dispatch(cmd._1(val !== null ? _Some(val) : _None));
        } catch (_) {
          dispatch(cmd._1(_None));
        }
        break;
      }

      case 'StorageSet':
        // _0: key (String), _1: value (String)
        try {
          localStorage.setItem(cmd._0, cmd._1);
        } catch (e) {
          console.error('TypedJS StorageSet failed:', e);
        }
        break;
    }
  }

  // ============= SUBSCRIPTION MANAGER =============

  /**
   * Manages the lifecycle of active subscriptions. Each subscription variant
   * produces a cleanup function when started. When the subscription set changes
   * (based on the current model), old subs are cleaned up and new ones started.
   */
  class SubscriptionManager {
    constructor() {
      this._active = new Map(); // key → { cleanup: Function }
    }

    /**
     * Diff the new subscription tree against active subscriptions.
     * Start new ones, stop removed ones.
     */
    update(sub, dispatch) {
      if (!sub) sub = { _tag: 'SubNone' };
      const leaves = this._flatten(sub);
      const newKeys = new Set();

      for (let i = 0; i < leaves.length; i++) {
        const key = this._key(leaves[i]);
        newKeys.add(key);
        if (!this._active.has(key)) {
          const cleanup = this._start(leaves[i], dispatch);
          this._active.set(key, { cleanup });
        }
      }

      // Stop subscriptions that are no longer present
      for (const [key, entry] of this._active) {
        if (!newKeys.has(key)) {
          entry.cleanup();
          this._active.delete(key);
        }
      }
    }

    /**
     * Flatten a Sub<Msg> tree (SubBatch, SubNone) into an array of leaf subs.
     */
    _flatten(sub) {
      if (!sub || sub._tag === 'SubNone') return [];
      if (sub._tag === 'SubBatch') {
        const result = [];
        for (let i = 0; i < sub._0.length; i++) {
          const inner = this._flatten(sub._0[i]);
          for (let j = 0; j < inner.length; j++) {
            result.push(inner[j]);
          }
        }
        return result;
      }
      return [sub];
    }

    /**
     * Generate a stable, deterministic key for a subscription leaf.
     * Two subs of the same variant (with same parameters) share a key
     * so they don't get duplicated.
     */
    _key(sub) {
      switch (sub._tag) {
        case 'Every':           return 'Every:' + sub._0;
        case 'OnAnimationFrame': return 'OnAnimationFrame';
        case 'OnWindowResize':  return 'OnWindowResize';
        case 'OnKeyPress':      return 'OnKeyPress';
        default:                return sub._tag;
      }
    }

    /**
     * Start a subscription and return its cleanup function.
     */
    _start(sub, dispatch) {
      switch (sub._tag) {
        case 'Every': {
          // _0: interval (Int ms), _1: mapper (Float -> Msg)
          const id = setInterval(() => {
            dispatch(sub._1(performance.now()));
          }, sub._0);
          return () => clearInterval(id);
        }

        case 'OnAnimationFrame': {
          // _0: mapper (Float -> Msg)
          let running = true;
          let rafId;
          const loop = (timestamp) => {
            if (!running) return;
            dispatch(sub._0(timestamp));
            rafId = requestAnimationFrame(loop);
          };
          rafId = requestAnimationFrame(loop);
          return () => {
            running = false;
            cancelAnimationFrame(rafId);
          };
        }

        case 'OnWindowResize': {
          // _0: curried mapper (Int -> Int -> Msg)
          // Since TypedJS has no tuples, this is curried: f(width)(height)
          const handler = () => {
            dispatch(sub._0(window.innerWidth)(window.innerHeight));
          };
          window.addEventListener('resize', handler);
          return () => window.removeEventListener('resize', handler);
        }

        case 'OnKeyPress': {
          // _0: mapper (Int -> Msg)
          const handler = (e) => {
            dispatch(sub._0(e.keyCode));
          };
          document.addEventListener('keydown', handler);
          return () => document.removeEventListener('keydown', handler);
        }

        default:
          return () => {};
      }
    }
  }

  // ============= DOM QUERY BOUNDARY =============
  //
  // These functions are the runtime implementations of the Dom.tjs
  // specifications. They wrap raw DOM queries in Option<DomElement>,
  // ensuring null/undefined never leaks into the typed world.

  function get_element_by_id(id) {
    const el = document.getElementById(id);
    return el ? _Some(el) : _None;
  }

  function query_selector(selector) {
    try {
      const el = document.querySelector(selector);
      return el ? _Some(el) : _None;
    } catch (_) {
      return _None;
    }
  }

  function get_attribute(el, name) {
    const val = el.getAttribute(name);
    return val !== null ? _Some(val) : _None;
  }

  function get_text_content(el) {
    const text = el.textContent;
    return text !== null ? _Some(text) : _None;
  }

  // ============= APPLICATION BOOTSTRAPPER =============

  /**
   * TypedJS.app — The main entry point for an Elm-architecture application.
   *
   * Config:
   *   init:          () => [model, cmd]
   *   update:        (msg, model) => [newModel, cmd]
   *   view:          (model) => Html<Msg>
   *   subscriptions: (model) => Sub<Msg>     (optional)
   *   root:          String (element ID) | HTMLElement
   *
   * Since TypedJS has no tuples, init and update return two-element JS arrays.
   *
   * Returns an object with:
   *   dispatch:  (msg) => void    — manually dispatch a message
   *   getModel:  () => model      — inspect current model (for debugging)
   */
  global.TypedJS = {
    app: function (config) {
      // Resolve root element
      const rootEl = typeof config.root === 'string'
        ? document.getElementById(config.root)
        : config.root;

      if (!rootEl) {
        throw new Error(
          'TypedJS.app: Root element not found: ' + JSON.stringify(config.root)
        );
      }

      // ---- State ----
      let currentModel;
      let currentVNode;
      let rootDomNode;
      const subManager = new SubscriptionManager();

      // ---- Batched Dispatch ----
      // Synchronous dispatches within the same microtask are batched
      // into a single render pass to avoid redundant DOM updates.
      let pendingMsgs = [];
      let isScheduled = false;

      function dispatch(msg) {
        pendingMsgs.push(msg);
        if (!isScheduled) {
          isScheduled = true;
          queueMicrotask(flush);
        }
      }

      function flush() {
        isScheduled = false;

        const msgs = pendingMsgs;
        pendingMsgs = [];

        if (msgs.length === 0) return;

        // Process all queued messages, accumulating model changes and commands
        let model = currentModel;
        const cmds = [];

        for (let i = 0; i < msgs.length; i++) {
          const result = config.update(msgs[i], model);
          model = result.model;   // new model
          cmds.push(result.cmd); // cmd
        }

        currentModel = model;

        // Re-render: view → diff → patch
        const newVNode = config.view(currentModel);
        const patches = diff(currentVNode, newVNode);

        if (patches) {
          const newRoot = applyPatches(rootDomNode, patches, dispatch);
          if (newRoot && newRoot !== rootDomNode) {
            rootDomNode = newRoot;
          }
        }

        currentVNode = newVNode;

        // Execute accumulated commands
        for (let i = 0; i < cmds.length; i++) {
          executeCmd(cmds[i], dispatch);
        }

        // Update subscriptions based on new model
        if (config.subscriptions) {
          subManager.update(config.subscriptions(currentModel), dispatch);
        }
      }

      // ---- Initialization ----
      const initResult = config.init();
      currentModel = initResult.model;
      const initialCmd = initResult.cmd;

      // First render
      currentVNode = config.view(currentModel);

      // Clear root
      while (rootEl.firstChild) {
        rootEl.removeChild(rootEl.firstChild);
      }

      rootDomNode = createElement(currentVNode, dispatch);
      rootEl.appendChild(rootDomNode);

      // Execute initial command
      executeCmd(initialCmd, dispatch);

      // Start initial subscriptions
      if (config.subscriptions) {
        subManager.update(config.subscriptions(currentModel), dispatch);
      }

      // Public API for debugging and external integration
      return {
        dispatch,
        getModel: () => currentModel,
      };
    },

    // Expose DOM query functions for use by compiled code
    dom: {
      get_element_by_id,
      query_selector,
      get_attribute,
      get_text_content,
    },
  };

})(typeof window !== 'undefined' ? window : this);
