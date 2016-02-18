var Inspector = Inspector || {};

(function(is) {
  var dbg_treeOps = false;
  var eg = EvolGo; // to be loaded before
  
  function infoForContext(sym_ctx) {
    return ("[ctx] " + stringOrChunks2span(sym_ctx, "term")
            + ", default: " + (sym_ctx.default ? "yes" : "no"));
  }
  function indirectCtxRefSym(sym) {
    return (sym.val.type === "context"
            && (sym.prefix != "MAIN" // sym not in MAIN ..
                || sym.val.term !== sym.term)); // .. or not a MAIN:Ctx sym
  }
  function indirectCtxRef_map(sid, map) {
    return indirectCtxRefSym(map[sid]);
  }
  function indirectCtxRef(sid) {
    return indirectCtxRefSym(symsMap[sid]);
  }
  function limitStrTo(str, limit) {
    return (str.length > limit
            ? str.substring(0, limit - 3) + "..."
            : str);
  }
  function limitStrTo(str, limit) {
    return (str.length > limit
            ? str.substring(0, limit) + "\n...[truncated]"
            : str);
  }

  // http://stackoverflow.com/questions/15815256/convert-multiple-spaces-to-nbsp-at-beginning-of-line-with-javascript
  function escapeSpaces (str) {
    return str.replace(/ +/g, function (match) {
      return match.replace(/ /g, "\u00A0"); // [sr] &nbsp -> \u00A0
    });
  }
  function escapeTabs (str) {
    return str.replace(/^\t+/mg, function (match) {
      return match.replace(/\t/g,
                           "\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0");
    });
  }
  function infoForString(string) {
    var chunks = string.rep;
    var all =
      chunks.join("\n")
      .replace(/</g, "&lt;").replace(/>/g, "&gt;")// avoid HTML embedding errors
      .replace(/ /g, "\u00A0"); // preserve spaces
    return (limitStrTo(all, 1024)
            .replace(/\n/g, "<br>")); // preserve NLs
  }

  function infoForLambdaOrMacro(lambdaOrMacro) {
    var str = lambdaOrMacro.val;
    return limitStrTo(str, 256);
  }
  function infoForListOrArray(loa) {
    var res;
    if (loa.type === "list") {
      res = "[list]";
    } else if (loa.type === "array") {
      res = "[array]";
    } else {
      loa = null;
      res = "[unknown type]";
    }
    if (loa) {
      res += " length: " + loa.length + ", val: " + limitStrTo(loa.rep, 1024);
    }
    return res;
  }
  function create_span(contentStr, classStr, titleStr) {
    return ('<span'
            + (classStr ? ' class="' + classStr + '"' : "")
            + (titleStr ? ' title="' + titleStr + '"' : "")
            + '>' + contentStr + '</span>');
  }
  function chunked2HTML(chunks) {
    var res = "";
    chunks.forEach(function(chunk, ix) {
      res += (ix % 2 === 1
              ? create_span(chunk,
                            "is_non-UTF-8",
                            "non-UTF-8: shown as decimal quoted byte(s)")
              : chunk);
    });
    return res;
  }

  //TODO
  // Sym contains prop or ( prop and prop_chunks ); prop_chunks are for
  // emphasizing quoted non-UTF-8 chars.
  function stringOrChunks2span(sym, prop) {
    var prop_chunks = prop + "_chunks";
    return (sym[prop_chunks]
            ? chunked2HTML(sym[prop_chunks]) // creates spans
            : sym[prop]); // no spans needed here
  }
  function propsForSym(sym) {
    return (
      (sym["global?"] || sym["protected?"] || ! sym["termString_legal?"]
       ? (" "
          + create_span("("
                        + (sym["protected?"] ? "P" : "")
                        + (sym["global?"] ? "G" : "")
                        + (sym["termString_legal?"] ? "" : "L̶")
                        + ")"
                        , "is_symProps"
                        , "P: proteced, G: global, L̶: term string not legal"))
       : ""));
  }
  function propsForDynsym(sym) {
    //todo: clearify if and what needed
    return (! sym["targetTermString_legal?"]
            ? (" "
               + create_span("("
                             + (sym["targetTermString_legal?"] ? "" : "L̶")
                             + ")"
                             , "is_dynsymProps"
                             , "L̶: target term string not legal"))
            : "");
  }
  function nameForPrefix(sym) {
    return stringOrChunks2span(sym, "prefix");
  }
  function nameForTerm(sym) {
    return stringOrChunks2span(sym, "term");
  }
  function nameForSym(sym) {
    return (stringOrChunks2span(sym, "prefix")
            + ":"
            +  stringOrChunks2span(sym, "term")
            + propsForSym(sym));
  }
  function nameForDynsym(sym) {
    var var_sid = getDynVarFrom_sym(sym);
    var var_sym = sid2sym(var_sid);
    var targetPrefix = (var_sym
                        && var_sym.type === "sym"
                        && var_sym.val.type === "context"
                        && var_sym.val.term);
    var targetCtx_sym = targetPrefix && symsMap["MAIN:" + targetPrefix];
    // use target context symbol's term info (may be chunks) as target prefix
    targetPrefix = targetCtx_sym && stringOrChunks2span(targetCtx_sym, "term");
    return ("" //"("
            + stringOrChunks2span(sym, "varPrefix")
            + ":"
            + stringOrChunks2span(sym, "varTerm")
            + "<br>→ "
            + (targetPrefix
               || ("[not a context ("
                   + (var_sym && var_sym.val.type || "???")
                   + ")]"))
            + ":" //"):"
            + stringOrChunks2span(sym, "targetTerm")
            + propsForDynsym(sym));
  }
  //todo: nested span mechanism here, too
  function infoForSym(sym) {
    return ("[sym] "
            + nameForSym(sym)
            + " [props] global?: "
            + (sym["global?"] ? "true" : "nil") + ", protected?: "
            + (sym["protected?"] ? "true" : "nil")
            + ", prefix: " + nameForPrefix(sym)
            + ", term: " + nameForTerm(sym));
  }
  function infoForDynsymVal(dsym) {
    var varTarget_sids = getDynVarTargetFrom_sym(dsym);
    var var_sid = varTarget_sids[0];
    var target_sid = varTarget_sids[1];
    var val = (target_sid && symsMap[target_sid]
               ? symsMap[target_sid].val
               : null);
    return (target_sid
            ? (val
               ? infoForVal(val)
               : "nil" + (symsMap[target_sid]
                          ? ""
                          : " [symbol does not exist]"))
            : "[cannot resolve " + var_sid + " to reach target symbol]");
  }
  function infoForVal(val) {
    switch (val.type) {
    case "nil": case "true": case "integer": case "float": case "bigint":
    case "primitive": case "quote":
      return val.val;
    case "sym":
      return infoForSym(val);
    case "string":
      return infoForString(val);
    case "context":
      return infoForContext(val);
    case "lambda":
      return infoForLambdaOrMacro(val);
    case "macro":
      return infoForLambdaOrMacro(val);
    case "list": case "array":
      return infoForListOrArray(val, val);
    default:
      return "missing case (implementation error)";
    }
  }

  var symsMap;
  var sid2node_ids = {};
  var sid2node_ids_nullCount = 0;
  var updateCount = 0;
  var renderCount = 0;
  // dyn sym deps
  var known_dsids = {};
  
  function symbols_eval2context(symsMap) {
    return eg.filterVals2arr(symsMap, function(sym) {
      return sym.prefix === "MAIN" && sym.val.type === "context";
    });
  }
  function symbols_inContext(symsMap, contextTerm) {
    return eg.filterVals2arr(symsMap, function(sym) {
      return sym.prefix === contextTerm;
    });
  }
  function pred_sym_eval2context_map(sid, map) {
    return map[sid].val.type === "context";
  }
  function create_pred_sym_inContext(contextTerm) {//contextTerm in MAIN ..
    return function(sid, map) { // .. is prefix of symbol
      return map[sid].prefix === contextTerm;
    }
  }
  function create_pred_sym_inContext_map(contextTerm, map) {
    return function(sid) {
      return map[sid].prefix === contextTerm;
    }
  }

  var pred_sym_inMAIN_map = create_pred_sym_inContext("MAIN");
  var pred_sym_inMAIN = null; // to be created after symsMap has been set
  function pred_context_inMAIN_map(sid, map) {
    return (pred_sym_eval2context_map(sid, map)
            && pred_sym_inMAIN_map(sid, map)
            && ! indirectCtxRef_map(sid, map));//exclude syms not standing for..
    // .. referenced context in MAIN itself
  }
  function pred_context_inMAIN(sid) {
    return pred_context_inMAIN_map(sid, symsMap);
  }
  function sort_syms(sid_1, sid_2) {
    var sym_1 = symsMap[sid_1], sym_2 = symsMap[sid_2];
    var prefix_1 = sym_1.prefix, prefix_2 = sym_2.prefix;
    var term_1 = sym_1.term, term_2 = sym_2.term;
    return (prefix_1 === prefix_2
            ? (term_1 === term_2
               ? 0
               : (term_1 < term_2 ? -1 : 1))
            : (prefix_1 < prefix_2 ? -1 : 1));
  };
  function sort_symsReversed(sid_1, sid_2) {
    return - sort_syms(sid_1, sid_2);
  };
  function sort_symsInContext(sid_1, sid_2) {
    return (symsMap[sid_1].prefix === symsMap[sid_1].term
            ? -1 // functor
            : (symsMap[sid_2].prefix === symsMap[sid_2].term
               ? 1
               : sort_syms(sid_1, sid_2)));
  };

  function classesForSym(sid) {
    var sym = symsMap[sid];
    var res = "";
    if (sym.type === "dynsym") {
      res += "dynsym";
      var target_sid = getDynTargetFrom_dsid(sid);
      var target = target_sid && symsMap[target_sid];
      res += " " + (target_sid
                    ? (target
                       ? target.val.type
                       : "dynsym_no-target_sym")
                    : "dynsym_no-target_resolve");
    } else {
      res += sym.val.type;
    }
    return res;
  }

  function isListSymMap(sid, map) {
    var sym = map[sid];
    var type = sym && sym.val && sym.val.type;
    return type && (type === "lambda" || type === "macro" || type === "list");
  }
  function isListSym(sid) {
    return isListSymMap(sid, symsMap);
  }
  function copyIntoSymsMap(childsData) {
    // copy new syms into symsMap (overwrite needed for updates in ping-pong
    // mode)
    for (var key in childsData) {
      symsMap[key] = childsData[key];
    }
  }

  var tree = null;
  var pingPongModeFlag = false;
  var nodeIx = 0;
  // caching works, if drag'n'drop is switched off; but nevertheless it remains
  // a hack, depending on jqtree implementation details ...
  var cacheLIs_Flag = true;
  var topFolders = [];
  var listFolderSyms = {};
  var contextFolderNode_ids = null;

  // Make sym id for a dynamic symbol.
  // Dynamic symbols don't appear as childs in any context folder, but may
  // appear in list folders (lambdas, macros).
  // There are dependencies sid -> dsids, which are handled by:
  // - mem'ing appearing dsids in known_dsids, and
  // - checking which and how known_dsids have to be updated after standard syms
  //   have been modified.
  function make_dsid(dsym) {
    return ("(" + dsym.varPrefix + ":" + dsym.varTerm + ")"
            + ":" + dsym.targetTerm);
  }
  function handle_sidOrDynsym(sidOrDynsym, ix) {
      if (typeof sidOrDynsym === 'string') {
        return sidOrDynsym; // ready to use sid
      }
      // dynsym_sid to be created
      var dsid = make_dsid(sidOrDynsym);
      symsMap[dsid] = sidOrDynsym; // overwriting identical val does not hurt
      known_dsids[dsid] = true; // overwriting identical val does not hurt
      return dsid;
  };
  function createTopFolder(sid, childsData) {
    //todo: normally childsData is a child_sids array...
    if (pingPongModeFlag) {
      copyIntoSymsMap(childsData);
    }
    //console.log(childsData);
    var child_sids = childsData.map(handle_sidOrDynsym);
    var childs = child_sids.map(registeredSymbolNodeData);
    var topNode = tree.tree('getTree').children[0];
    var node_id = ++nodeIx;
    add_node_id(sid, node_id);
    if (! is.createForContextsFlag) {
      topFolders.push(node_id);
      listFolderSyms[sid] = true;
    }
    var nodeData = symbolFolderNodeData(sid);
    nodeData.id = node_id;
    topNode = tree.tree('addNodeBefore', nodeData, topNode);
    tree.tree('loadData', childs, topNode);
    // Note: not suited for caching, due to sub DOM elements reflecting childs
    // data (UL); so only cache leave nodes.
  }

  function label_dynsym(dsym) {
    return ("[dyn] "
            + nameForDynsym(dsym));
  }
  function label_sym(sym) {
    return ((indirectCtxRefSym(sym) ? "[ref] " : "")
            + nameForSym(sym)
            + (sym.reference ? "<span> → " + sym.target + "</span>" : ""));
  }
  function labelForSym(sym) {
    return (sym.type === "sym"
            ? label_sym(sym)
            : label_dynsym(sym));
  }
  var symbolNodeDataCount = 0; //todo: stats object
  function symbolNodeData(sid) {
    var sym = symsMap[sid];
    var val = (sym.type === "sym"
               ? infoForVal(sym.val)
               : (sym.type === "dynsym"
                  ? infoForDynsymVal(sym)
                  : "missed case"));
    //eg.log("sid: ", sid, ", sym: ", sym);
    return { // constant after rendered once (as long there is no drag'n'drop)
      cacheLiFlag: cacheLIs_Flag, // OK for leave nodes (but only for them)
      modified: updateCount,
      sid: sid,
      ID: sid, // for anchor
      val: val,
      hash: sym.valHash,//todo for comparison, because val may be e.g. truncated
      label: (dbg_treeOps
              ? "(" + ++symbolNodeDataCount + ")"
              : ""), // label usually dummy: label content added later
      addLabelForSym_flag: true
    }
  }
  function registeredSymbolNodeData(sid) {
    var node_ids = sid2node_ids[sid];
    var node_id = (node_ids && node_ids.indexOf(sid) >= 0
                   ? ++nodeIx // already exists
                   : sid); // first time
    add_node_id(sid, node_id);
    var node = symbolNodeData(sid);
    node.id = node_id;
    return node;
  }
  function folderNodeData(sid) {
    var nodeData = {
      // without: id: node_id,
      modified: updateCount,
      sid: sid,
      ID: sid, // list sym id, if not context folder; else context name
      folderFlag: true, // may be folder without having child nodes
    };
    return nodeData;
  }
  function symbolFolderNodeData(sid) {
    var nodeData = folderNodeData(sid);
    var sym = symsMap[sid];
    nodeData.label = '[' + sym.val.type + '] ';//rm + nameFor(sid);
    nodeData.symbolFolderFlag = true;
    nodeData.val = infoForVal(sym.val);
    nodeData.addLabelForSym_flag = true;
    // no caching (no cacheLiFlag set)
    return nodeData;
  }
  function contextFolderNodeData(sid, initialLoadFlag) {
    var nodeData = folderNodeData(sid);
    var sym = symsMap[sid];
    nodeData.label = "[ctx] " + nameForTerm(sym);
    nodeData.contextFolderFlag = true;
    // Note: not suited for caching, due to sub DOM elements reflecting childs
    // data (UL); so just cache leave nodes.
    return nodeData;
  }

  
  // term quoted in case of control chars
  function registeredContextFolderNodeData(sid_context, initialLoadFlag) {
    var sym = symsMap[sid_context];
    var node_id = sym.term;
    add_node_id(sid_context, node_id); // register
    var nodeData = contextFolderNodeData(sid_context, initialLoadFlag);
    nodeData.id = node_id;
    return nodeData;
  }
  // unused
  // clone props selected by keys arr
  function cloneSelectedProps_obj_keys(obj, keys) {
    var ix, len = keys.length;
    var clone = {};
    for (ix = 0; ix < len; ++ix) {
      var key = keys[ix];
      clone[key] = obj[key];
    }
    return clone;
  }
  function createCloneFrom(node) {
    return cloneSelectedProps_obj_keys(
      node,
      ["id", "sid", "ID",
       // "val", "label", "cachedLiFlag",
       "folderFlag", "contextFolderFlag",
       "hash"]
    );
  }

  function getNode(node_id) {
    return tree.tree('getNodeById', node_id);
  }
  function tree_getState() {
    return tree.tree('getState');
  }
  function node_ID(node_id) {
    return getNode(node_id).ID;
  }
  function node_sid(node_id) {
    return getNode(node_id).sid;
  }
  is.node_ID = node_ID; // export
  var containsSymbols = isListSym;
  // reuse oldNode.id
  // caching dangerous for to be replaced nodes
  // function tree_replaceNode_withCaching(oldNode, newNodeData, newChildsData) {
  //   is.log_dbgSTO("[tree_replaceNode()] sid: " + oldNode.sid
  //                 + ", id: " + oldNode.id);
  //   openFlag = tree.tree('getState').open_nodes.indexOf(oldNode.id) >= 0;
  //   newNodeData.cacheLiFlag = false;
  //   var newNode = tree.tree('addNodeAfter', newNodeData, oldNode);
  //   tree.tree('removeNode', oldNode);
  //   // id has to be set *after* removing node with same id
  //   tree.tree('updateNode', newNode, { id: oldNode.id });
  //   newNode.cacheLiFlag = cacheLIs_Flag; // will be rendered once from now
  //   if (newChildsData) {
  //     tree.tree('loadData', newChildsData, newNode)
  //   }
  //   if (openFlag) {
  //     tree.tree('openNode', newNode, false);
  //   }
  // }
  function tree_replaceNode(oldNode, newNodeData, newChildsData) {
    is.log_dbgSTO("[tree_replaceNode() 3] sid: " + oldNode.sid
                  + ", id: " + oldNode.id);
    openFlag = tree.tree('getState').open_nodes.indexOf(oldNode.id) >= 0;
    tree.tree('updateNode', oldNode, newNodeData);
    if (newChildsData) {
      tree.tree('loadData', newChildsData, oldNode)
    }
    if (openFlag) {
      tree.tree('openNode', oldNode, false);
    }
  }
  function tree_updateNode(oldNode, newNodeData) {
    is.log_dbgSTO("[tree_updateNode()] sid: " + oldNode.sid
                  + ", id: " + oldNode.id);
    var stored_cacheLiFlag = newNodeData.cacheLiFlag;
    newNodeData.cacheLiFlag = false; // enforce rerendering ..
    tree.tree('updateNode', oldNode, newNodeData);
    oldNode.cacheLiFlag = stored_cacheLiFlag; // .. cache again (if switched on)
  }
  // Updates of symbol nodes and list symbol folders.
  function updateNode(old_node) {
    if (old_node.contextFolderFlag) {
      return; // updated outside here
    }
    var sid = old_node.sid;
    is.log_dbgSTO("[updateNode()] sid: " + sid
                  + ", id: " + old_node.id);
    if (old_node.modified && old_node.modified === updateCount) {
      is.log_dbgSTO("[updateNode()] -> already current (nothing to do).");
      return;
    }
    if (isListSym(sid)
        && old_node.symbolFolderFlag) { // has been a list symbol folder before
      var old_childNodes = old_node.children;
      var old_child_sids = old_childNodes.map(function(childNode) {
        childNode.sid;
      });
      var new_child_sids = listFolderSym2Childs[sid];
      var new_folderData = symbolFolderNodeData(sid);
      if (arraysStrictEqualPred(old_child_sids, new_child_sids)) { // update ..
        tree_updateNode(old_node, new_folderData); //.. of childs at other place
      } else { // replace
        deregisterChilds(old_node);
        var new_childsData = new_child_sids.map(registeredSymbolNodeData);
        var new_folderNode = tree_replaceNode(old_node,
                                              new_folderData, new_childsData);
      }
    } else { // not a list sym folder before and now
      var newNodeData = symbolNodeData(sid);
      if (old_node.folderFlag) {
        deregisterChilds(old_node);
        // does not work by changing from folder to non-folder
        //   tree.tree('updateNode', old_node, newNode);
        // no node deregistering, id reused
        tree_replaceNode(old_node, newNodeData);
      } else { // old_node not a folder
        tree_updateNode(old_node, newNodeData);
      }
    }
  }
  function updateNodesFor(sid) {
    sid2node_ids[sid].slice().forEach(function(node_id) {
      updateNode(getNode(node_id));
    });
  }
  function arraysStrictEqualPred(arr1, arr2) {
    var len1 = arr1.length;
    if (len1 !== arr2.length) {
      return false;
    }
    for (var ix = 0; ix < len1; ++ix) {
      if (arr1[ix] !== arr2[ix]) {
        return false;
      }
    }
    return true;
  }
  function folderInfo(folders) {
    is.log_dbgSTO(folders.length
                  ? ("folder info:"
                     + folder.map(function(node) {
                       return "\n  sid: " + node.sid
                         + ", id: " + node.id
                         + ", ID: " + node.ID;
                     }))
                  : "no folders");
  }
  function updateTopFolder(node_id) {
    var node = getNode(node_id);
    node.children.map(function(childNode) {
      updateNode(childNode);
    });
  }
  function updateListFolder(node_id, data) {
    copyIntoSymsMap(data);
    updateTopFolder(node_id);
  }

  function add_node_id(sid, node_id) {
    var arr = sid2node_ids[sid];
    if (! arr) {
      arr = sid2node_ids[sid] = []; // create
    }
    arr.push(node_id);
  }
  function remove_node_id(sid, node_id) {
    //is.log_dbgSTO("[remove_node_id] sid: " + sid + ", node_id: " + node_id);
    var arr = sid2node_ids[sid]; eg.assert(arr);
    var ix = arr.indexOf(node_id); eg.assert(ix !== -1);
    arr.splice(ix, 1);
    if (! arr.length) { //todo: cleanup for all at end of update cycle ..
      sid2node_ids[sid] = null; // .. or delete here
      ++sid2node_ids_nullCount;
      console.log("sid2node_ids_nullCount:", sid2node_ids_nullCount);
    }
  }
  function createContextNodesData(allSyms, initialLoadFlag, sid_context) {
    if (sid_context === "MAIN") {
      //console.log(symsMap);
    }
    var sym = symsMap[sid_context];
    var registeredFolderNodeData =
      registeredContextFolderNodeData(sid_context, initialLoadFlag);
    var symsInContext =
      allSyms.filter(create_pred_sym_inContext_map(sym.term, symsMap))
      .sort(sort_symsInContext);
    var registeredSymbolNodesData =
      symsInContext.map(registeredSymbolNodeData);
    if (initialLoadFlag) {
      registeredFolderNodeData.children = registeredSymbolNodesData;
      return registeredFolderNodeData;
    } else {
      return [ registeredFolderNodeData, registeredSymbolNodesData ];
    }
  }
  function createContextFolder(allSyms, sid_context) {
    var contextNodesData = createContextNodesData(allSyms, false,
                                                  sid_context);
    var folderNodeData = contextNodesData[0];
    var childNodesData = contextNodesData[1];
    var node_id = folderNodeData.id;
    var ix = 0, len = contextFolderNode_ids.length;
    while (ix < len && contextFolderNode_ids[ix] < node_id) {
      ++ix;
    }
    var succNode, predNode, newNode;
    if (ix < len) {
      succNode = getNode(contextFolderNode_ids[ix]);
      newNode = tree.tree('addNodeBefore', folderNodeData, succNode);
    } else {
      predNode = getNode(contextFolderNode_ids[len-1]);
      newNode = tree.tree('addNodeAfter', folderNodeData, predNode);
    }
    contextFolderNode_ids.splice(ix, 0, node_id);
    tree.tree('loadData', childNodesData, newNode);
  }

  function cleanupIfNeeded() {
    if (! tree.tree('getTree')) {
      return;
    }
    symsMap = null; // cannot hurt
    known_dsids = {};
    allSyms = null; // cannot hurt
    pred_sym_inMAIN = null; // cannot hurt (to be recreated)
    sid2node_ids = {};
    sid2node_ids_nullCount = 0;
    updateCount = 0;
    renderCount = 0;
    nodeIx = 0;
    topFolders = [];
    listFolderSyms = {};
    contextFolderNode_ids = null;
  }
  var onCreateLiCount = 0;
  // success func for $.getJSON() taking all symbols data
  function createInitialContextFolders(data) {
    cleanupIfNeeded();
    is.createForContextsFlag = true;
    symsMap = data;
    pred_sym_inMAIN = create_pred_sym_inContext_map("MAIN", symsMap);
    allSyms = eg.keys(symsMap);
    var contextSyms =
      allSyms
      .filter(pred_context_inMAIN)
      .sort(sort_syms);
    var nodesData = contextSyms.map(eg.bindThisNArgs(createContextNodesData,
                                                     null, allSyms, true));
    contextFolderNode_ids = nodesData.map(function(nodeData) {
      return nodeData.id;
    });
    // console.log("getTree 1", tree.tree('getTree'));
    if (tree.tree('getTree')) {
      tree.tree('loadData', nodesData);
    } else {
      tree.tree({
        data: nodesData,
        onCreateLi: function(node, $li) {
          is.log_dbgSTR("onCreateLi: " + node.sid + " " + node.id);
          ++renderCount;
          var titleElem = $li.find('.jqtree-title');
          titleElem.addClass('title');
          // add anchor
          if (node.ID) {
            titleElem.before('<a name="' + node.ID + '"></a>');
          }
          if (node.addLabelForSym_flag) {
            titleElem.append(labelForSym(symsMap[node.ID]));
          }
          $li.addClass("inspector");
          if (node.folderFlag) {
            $li.addClass("folder");
          }
          //eg.log("node.id: ", node.id, ", node.ID: ", node.ID);
          if (! node.contextFolderFlag) { // list sym or list sym folder
            $li.addClass(classesForSym(node.sid));
          }
          if (node.val) {
            titleElem.after(
              ""+ '<span class="inspector valChar">'
                + (dbg_treeOps
                   ? "" + ++onCreateLiCount + "."
                   : "")
                + (node.symbolFolderFlag
                   ? '→'
                   : '→')
                + '</span>'
                + '<span class="val">'
                + node.val //todo?: nested span mechanism here, too
                + '</span>');
          }
          // for more:
          //   $li.attr('id', node.id);
          //   $li.find('.jqtree-title').before('<a name="bar"></a>');
        },
        dragAndDrop: false // false allows selection of text, and caching of LIs
      });
    }
    // console.log("getTree 2", tree.tree('getTree'));

    // from here on all symname IDs are exhausted, but jqtree needs unique
    // ones:
    is.createForContextsFlag = false; // this flag states this (todo)
    //eg.log("createInitialContextFolders()", sid2node_ids);
  }
  function init_setSymbolsTreeSelector(selector) {
    tree = $(selector);
  }
  function setPingPongMode() {
    pingPongModeFlag = true;
  }
  function init_honorHash() {
    // open its parent folder, if child node, and scroll to it
    $(window).on('hashchange', function(ev) {
      var node = getNode(location.hash.substring(1));
      //is.log_info && is.log_info("hashchange, node: " + node);
      if (! node) {
        if (window.location.hash) { // avoid '#' without anchor in URL
          window.location.hash = window.location.hash; // for jumping to #help
        }
        return; // avoid referring to non-existing node
      }
      var change;
      for (var n = node.parent; n; n = n.parent) {
        if (n.folderFlag) {
          tree.tree('openNode', n);
          change = true;
        };
      }
      // timeout needed to reach the correct position
      setTimeout(function() {
        tree.tree('scrollToNode', node); // one above pos
        tree.tree('selectNode', node);
        //window.location.hash = window.location.hash; // browser scrolls to pos
      }, 100);
      
    });
  }
  // binsearch
  // <  --> < < <   == == == X > > >
  // <= --> < < < X == == ==   > > >
  function findInsertPos(elem, arr, cmpFun) {
    var begin = 0, end = arr.length, len, pos;
    if (! arr) {
      is.log_err("[implementation] array expected");
      return;
    }
    while (true) { // invariant: begin <= end
      if (begin === end) {
        return begin; // OK for empty arr
      }
      pos = begin + Math.trunc((end - begin) / 2);
      if (cmpFun(elem, arr[pos])) {
        end = pos;
      } else {
        begin = pos + 1;
      }
    }
  }
  is.findInsertPos = findInsertPos; // testing      
  function deregisterChilds(node) {
    node.children // node (folder, too) may have no children
      && node.children.forEach(function(childNode) {
        remove_node_id(childNode.sid, childNode.id); // mark childs as ..
      });
  }
  function removeChilds(node) {
    node.children // node (folder, too) may have no children
      && node.children.slice().forEach(function(childNode) {
        tree.tree('removeNode', childNode);
        remove_node_id(childNode.sid, childNode.id); // mark childs as ..
      });
  }
  function removeNode(node_id) {
    var node = getNode(node_id);
    //console.log("node_id: ", node_id, ", node: ", node);
    if (node) { // may have been deleted by deleting parent folder
      is.log_info("removeNode " + node_id
                  + (node_id !== node.ID ? "(" + node.ID + ")" : "")
                  + " -> OK.");
      var sid = node.sid;
      if (node.contextFolderFlag) {
        //console.log("contextFolderNode_ids.find(node_id):");
        var pos = contextFolderNode_ids.indexOf(node_id);
        //console.log(" pos: ", pos);
        contextFolderNode_ids.splice(pos, 1);
      } else if (node.folderFlag) { // remove from top ..
        topFolders.splice(topFolders.indexOf(node_id), 1); // .. folders
        var topFoldersForSym = topFolders.filter(function(node_id) {
          return getNode(node_id).sid === sid;
        });
        if (topFoldersForSym.length === 0) { // no topFolders for sym left
          // keys used as set (so = null is not an option)
          delete listFolderSyms[sid];
        }
      }
      deregisterChilds(node);
      // .. being removed, ..
      tree.tree('removeNode', node); //.. due to removing parent node here
      remove_node_id(sid, node_id);
    } else {
      is.log_info("remove node " + node.ID + " -> already done.");
    }
  }
  function removeNodesFor(sid) {
    var node_ids = sid2node_ids[sid];//no change of iterated coll below!
    if (! node_ids) {
      is.log_info("[remove] nodes for " + sid + " already removed.");
      return;
    } // todo: rm if finished
    // folders first should save ..
    //&&&
    node_ids.sort(function(node_id_1, node_id_2) { // .. some jqtree effort
      var node_1 = getNode(node_id_1);
      var node_2 = getNode(node_id_2);
      return (node_1.folderFlag
              ? (node_2.folderFlag
                 ? 0
                 : -1)
              : (node_2.folderFlag
                 ? 1
                 : 0));
    });
    // slice() for not changing iterated arr below!
    node_ids.slice().forEach(removeNode);
  }
  function deleteContextFoldersFor(sid) {
    contextFolderNode_ids.filter(function(node_id) {
      return getNode(node_id).sid === sid;
    }).forEach(removeNode);
  }

  function addToContextFolder(sid, nid_context) {
    var contextNode = getNode(nid_context);
    if (contextNode.modified === updateCount) {
      return; // added before as child of just created context folder
    }
    var childNodeData = registeredSymbolNodeData(sid);
    var children = contextNode.children;
    if (! children || ! children.length) {
      tree.tree('appendNode', childNodeData, contextNode);
    } else {
      pos = findInsertPos(sid, children, function(sid, childNode) {
        return sid <= childNode.sid;
      });
      var nodeAtPos = children[pos < children.length ? pos : pos - 1];
      tree.tree((pos < children.length
                 ? 'addNodeBefore'
                 : 'addNodeAfter'),
                childNodeData,
                nodeAtPos);
    }
  }
  // expects new contexts coming first
  function addContextNodesFor(sid) {
    is.log_dbgSTO("addContextNodesFor(): " + sid);
    if (pred_context_inMAIN(sid)) {
      allSyms = allSyms || eg.keys(symsMap);
      createContextFolder(allSyms, sid);
      addToContextFolder(sid, "MAIN");
    } else {
      addToContextFolder(sid, symsMap[sid].prefix);
    }
  }
  function cmp_symInMAIN_before(sid_1, sid_2) {
    return (pred_sym_inMAIN(sid_1)
            ? (pred_sym_inMAIN(sid_2)
               ? 0
               : -1)
            : (pred_sym_inMAIN(sid_2)
               ? 1
               : 0));
  }

  // check for sid in quoted string rep
  function isQuoted(sid) {
    var sym = symsMap[sid];
    return sym.prefix_chunks || sym.term_chunks;//todo: introduce prop "quoted?"
  }
  function quote_sid(sid) {
    return (isQuoted(sid)
            ? '"\'' + sid + '"'
            : "'" + sid);
  }
  is.quote_sid = quote_sid; // export
  function quote_sids(sids) {
    return sids.map(function(sid, ix) {
      return quote_sid(sid);
    });
  }
  // objs
  var modfied; // sid -> true
  var old_removed, new_added;   // sid -> sym
  var old_changed, new_changed; // sid -> sym
  // obj
  var listFolderSym2Childs;
  // arrs
  var allSyms;
  var removedSyms, addedSyms, changedSyms;
  // arrs
  var changedListFolderSymsWithChilds;
  
  function isDynsym(sid) {
    return symsMap[sid].type === "dynsym";
  }
  is.isDynsym = isDynsym; // export
  function updateSymbolsView(data) {
    ++updateCount;
    removedSyms = [], addedSyms = [], changedSyms = [];
    old_removed = {}, old_changed = {}, new_added = {}, new_changed = {};
    allSyms = null;
    modified = {};
    changedListFolderSymsWithChilds = []; // sids
    eg.forEach(data, function(val, sid) {
      if (! symsMap[sid]) {
        modified[sid] = true;
        addedSyms.push(sid);
        new_added[sid] = val;
      } else {
        var old = symsMap[sid];
        if (! eg.propsSameDeep(old, val)) {
          modified[sid] = true;
          old_changed[sid] = old;
          changedSyms.push(sid);
          new_changed[sid] = val;
        }
      }
    });
    eg.forEach(symsMap, function(val, sid) {
      if (! data[sid]
          && ! isDynsym(sid)) { // dynsyms are not visible in data
        modified[sid] = true;
        removedSyms.push(sid);
        old_removed[sid] = val;
      }
    });
    changedListFolderSymsWithChilds =
      eg.keys(listFolderSyms)
      .filter(function(sid) {
        return old_changed[sid] ? true : false;
      })
      .filter(function(sid) {
        return isListSymMap(sid, new_changed);
      });
    is.log_info("\n  added symbols  : " + JSON.stringify(addedSyms) +
                "\n  removed symbols: " + JSON.stringify(removedSyms) +
                "\n  changed symbols: " + JSON.stringify(changedSyms));
    if (changedListFolderSymsWithChilds.length) {
      //var listSymsExpr =
      //  "'('" + changedListFolderSymsWithChilds.join(" '") + ")";
      var listSymsExpr =
        "'(" + quote_sids(changedListFolderSymsWithChilds).join(" ") + ")";
      is.performIntrospection_symbolsInListsQuoted(listSymsExpr,
                                                   updateSymbolsViewAction);
    } else { // no further introspection needed
      updateSymbolsViewAction();
    }
  }
  function sid2sym(sid) {
    return sid && symsMap[sid];
  }
  function getDynVarFrom_sym(dsym) {
    return (dsym.varPrefix && dsym.varTerm
            && dsym.varPrefix + ":" + dsym.varTerm);
  }
  function getDynVarTargetFrom_sym(dsym) {
    var var_sid = getDynVarFrom_sym(dsym);
    var var_sym = var_sid && symsMap[var_sid];
    var target_sid = (var_sym
                      && var_sym.type === "sym"
                      && var_sym.val.type === "context"
                      && var_sym.val.term + ":" + dsym.targetTerm);
    return [var_sid, target_sid];
  }
  function getDynVarTargetFrom_dsid(dsid) {
    return getDynVarTargetFrom_sym(symsMap[dsid]);
  }
  function getDynTargetFrom_dsid(dsid) {
    return getDynVarTargetFrom_dsid(dsid)[1];
  }
  is.getDynTargetFrom_dsid = getDynTargetFrom_dsid; // export
  function compute_toBeUpdatedDynsyms(known_dsids, modified_sids) {
    var toBeUpdatedDynsyms = {};
    for (var dsid in known_dsids) {
      var vt = getDynVarTargetFrom_dsid(dsid);
      var var_sid = vt[0];
      var target_sid = vt[1];
      if (modified_sids[var_sid] || modified_sids[target_sid]) {
        toBeUpdatedDynsyms[dsid] = true;
      }
    };
    return toBeUpdatedDynsyms;
  }
  
  function updateSymbolsViewAction(dataOrNil) { // dataOrNil may be undefined
    renderCount = 0;
    //console.log(dataOrNil);
    listFolderSym2Childs = {};
    // dataOrNil undefined, if no changed list folder syms -> no loop
    for (var lsid in dataOrNil) {
      listFolderSym2Childs[lsid] = dataOrNil[lsid].map(handle_sidOrDynsym);
    };
    toBeUpdatedDynsyms = compute_toBeUpdatedDynsyms(known_dsids, modified);
    is.log_info("\n  to be updated dynamic symbols: "
                + JSON.stringify(eg.keys(toBeUpdatedDynsyms)));
    //console.log("listFolderSym2Childs:", dataOrNil);
    removedSyms.sort(cmp_symInMAIN_before).forEach(function(sid, ix) {
      removeNodesFor(sid);
      delete symsMap[sid];
    });
    addedSyms.forEach(function(sid, ix) {
      symsMap[sid] = new_added[sid];
    });
    changedSyms.forEach(function(sid, ix) {
      symsMap[sid] = new_changed[sid];
    });
    addedSyms.sort(cmp_symInMAIN_before);
    addedSyms.forEach(function(sid, ix) {
      addContextNodesFor(sid);
    });
    
    var changedToNotContextSyms = changedSyms.filter(function(sid) {
      return (pred_context_inMAIN_map(sid, old_changed)
              && ! pred_context_inMAIN(sid));
    });
    if (changedToNotContextSyms.length) {
      changedToNotContextSyms.forEach(function(sid, ix) {
        deleteContextFoldersFor(sid);
      });
    }
    var changedToNewContextSyms = changedSyms.filter(function(sid) {
      return (pred_context_inMAIN(sid)
              && ! pred_context_inMAIN_map(sid, old_changed));
    });
    if (changedToNewContextSyms.length) {
      allSyms = allSyms || eg.keys(symsMap);
      changedToNewContextSyms.forEach(function(sid, ix) {
        createContextFolder(allSyms, sid);
      });
    }
    
    // sort changed list folder syms to front, to avoid needless updating of
    // their old childs [todo: think]
    if (changedListFolderSymsWithChilds.length) {
      is.log_dbgSTO("before sort: " + changedSyms);
      changedSyms.sort(function(sid_1, sid_2) {
        return (listFolderSym2Childs[sid_1]
                ? (! listFolderSym2Childs[sid_2]
                   ? -1 : 0)
                : (listFolderSym2Childs[sid_2]
                   ? 1 : 0));
      });
      is.log_dbgSTO("after sort: " + changedSyms);
    }
    changedSyms.forEach(function(sid, ix) {
      updateNodesFor(sid);
    });
    for (var dsid in toBeUpdatedDynsyms) {
      if (! sid2node_ids[dsid]) { // last dsid sym node has gone
        delete known_dsids[dsid];
        // not reached by standard syms delete
        delete symsMap[dsid]; // not needed anymore (may exist further or not)
      } else { // update dsym nodes inside list folders
        updateNodesFor(dsid); // .. expects entry in symsMap
      }
    };
    is.log_dbgSTO("renderCount: " + renderCount);
    //
    if (addedSyms
        && new_added[location.hash.substr(1)]) {
      console.log("hash");
      $(window).trigger('hashchange');
    }
  }

  //
  // exports
  
  // get current symsMap
  function get_symsMap() {
    return symsMap;
  }
  is.createForContextsFlag = true; // init and export
  is.isListSym = isListSym;
  is.copyIntoSymsMap = copyIntoSymsMap;
  is.topFolders = topFolders; // arr
  is.createTopFolder = createTopFolder;
  is.tree_getState = tree_getState;
  is.updateTopFolder = updateTopFolder;
  is.createInitialContextFolders = createInitialContextFolders;
  is.init_setSymbolsTreeSelector = init_setSymbolsTreeSelector;
  is.setPingPongMode = setPingPongMode;
  is.init_honorHash = init_honorHash;
  is.updateListFolder = updateListFolder;
  is.updateSymbolsView = updateSymbolsView;
  is.sid2node_ids = sid2node_ids;
  // exports devel
  is.remove_node_id = remove_node_id;
  is.get_symsMap = get_symsMap;
}(Inspector));

/* Example as well as jqTree from:
 *   https://mbraak.github.io/jqTree/
 * .
 */
/*
var data = [
    {
        label: 'node1',
        children: [
            { label: 'child1' },
            { label: 'child2' }
        ]
    },
    {
        label: 'node2',
        children: [
            { label: 'child3' }
        ]
    }
];
$(function() {
    $(treeSel).tree({
        data: data
    });
});
*/
// EOF
