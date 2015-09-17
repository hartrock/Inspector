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
    $('#tree1').tree({
        data: data
    });
});
*/

var eg = EvolGo;

function infoForContext(context) {
  return "[ctx] " + context.term + " , default: " + context.default;
}
function indirectCtxRefSym(scobj) {
  return scobj.val.type === "context" && scobj.val.term !== scobj.term;
}
function indirectCtxRef(sid) {
  return indirectCtxRefSym(symsMap[sid]);
}
function limitStrTo(str, limit) {
  return (str.length > limit
          ? str.substring(0, limit - 3) + "..."
          : str);
}

// http://stackoverflow.com/questions/15815256/convert-multiple-spaces-to-nbsp-at-beginning-of-line-with-javascript
function escapeSpaces (str) {
    return str.replace(/^ +/mg, function (match) {
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
  var str = string.rep;
  return ((string["unprintable chars"] ? "[unprintable chars] " : "")
          // replace spaces and tabs at beginning of line to preserve indent in
          // span (.replace(/^ /gm, "\u00A0") is too much, due to not allowing
          // linebreaks for long lines)
          + escapeTabs(escapeSpaces(limitStrTo(str, 1024)))
          // avoid HTML embedding errors
          .replace(/</g, "&lt;").replace(/>/g, "&gt;")
          // preserve NLs
          .replace(/\n/g, "<br>")
         );
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
function infoForSym(sym) {
  return ("[sym] " + sym.prefix + ":" + sym.term + " [props] global?: "
          + (sym["global?"] ? "true" : "nil") + ", protected?: "
          + (sym["protected?"] ? "true" : "nil") + ", prefix: " + sym.prefix
          + ", term: " + sym.term);
}
/* older version of next func: it's almost ASCII art... */
/*
function infoForVal(val) {
  var res = (
    val.type === "nil"
      ? val.val
      : (val.type === "true"
         ? val.val
         : (val.type === "sym"
            ? infoForSym(val)
            : (val.type === "integer"
               ? val.val
               : (val.type === "string"
                  ? infoForString(val)
                  : (val.type === "context"
                     ? infoForContext(val)
                     : (val.type === "lambda"
                        ? infoForLambdaOrMacro(val)
                        : (val.type === "macro"
                           ? infoForLambdaOrMacro(val)
                           : (val.type === "float"
                              ? val.val
                              : (val.type === "list" || val.type === "array"
                                 ? infoForListOrArray(val)
                                 : (val.type === "bigint"
                                    ? val.val
                                    : (val.type === "primitive"
                                       ? val.val
                                       : (val.type === "quote"
                                          ? val.val
                                          : "missing case (implementation error)")))))))))))));
  return res;
}
*/
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

function symbols_eval2context(symsMap) {
  return eg.filterVals2arr(symsMap, function(symObj) {
    return symObj.prefix === "MAIN" && symObj.val.type === "context";
  });
}
function symbols_inContext(symsMap, contextTerm) {
  return eg.filterVals2arr(symsMap, function(symObj) {
    return symObj.prefix === contextTerm;
  });
}
function pred_sym_eval2context(id) {
  return symsMap[id].val.type === "context";
}
function create_pred_sym_inContext(contextTerm) {
  return function(id) {
    return symsMap[id].prefix === contextTerm;
  }
}
function sort_syms(id_1, id_2) {
  var sym_1 = symsMap[id_1], sym_2 = symsMap[id_2];
  var prefix_1 = sym_1.prefix, prefix_2 = sym_2.prefix;
  var term_1 = sym_1.term, term_2 = sym_2.term;
  return (prefix_1 === prefix_2
          ? (term_1 === term_2
             ? 0
             : (term_1 < term_2 ? -1 : 1))
          : (prefix_1 < prefix_2 ? -1 : 1));
};
function sort_symsReversed(id_1, id_2) {
  return - sort_syms(id_1, id_2);
};
function sort_symsInContext(id_1, id_2) {
  return (symsMap[id_1].prefix === symsMap[id_1].term
          ? -1 // functor
          : (symsMap[id_2].prefix === symsMap[id_2].term
             ? 1
             : sort_syms(id_1, id_2)));
};

function classesForVal(valObj) {
  return "inspector " + valObj.type;
}

function isListSym(id) {
  var symObj = symsMap[id];
  var type = symObj && symObj.val.type;
  return type && (type === "lambda" || type === "macro" || type === "list");
}

function copyIntoSymsMap(childsData) {
  // copy new syms into symsMap (overwrite needed for updates in ping-pong mode)
  for (var key in childsData) {
    if (! symsMap[key]) {
      unusedIDs[key] = true;
    }
    symsMap[key] = childsData[key];
  }
}
var nodeIx = 0;
var unusedIDs = {};
var cacheLIs_Flag = true; // caching works, if drag'n'drop is switched off
var topFolders = [];
function createFolder(ID, childsData) {
  copyIntoSymsMap(childsData);
  var sym = symsMap[ID];
  var type = sym.val.type;
  additionalMap = childsData;
  var childs = eg.map2arr(eg.keys(childsData), createChild);
  //eg.log("childs: ", childs);
  var node = $('#tree1').tree('getTree').children[0];
  var newNode_id = ++nodeIx;
  if (! createForContextsFlag) {
    topFolders.push(newNode_id);
  }
  var newNodeData = {
    ID: ID, // list sym ID, if not context folder; else context name
    id: newNode_id,
    folderFlag: true,
    label: '[' + type + '] ' + ID
  };
  // first LI rendered by adding node not suited for caching, but ..
  $('#tree1').tree('addNodeBefore', newNodeData, node);
  node = $('#tree1').tree('getNodeById', newNode_id); // node !== newNodeData
  // .. second LI will be rendered *after* loading childs data ..
  node.cacheLiFlag = cacheLIs_Flag; //.. (which leads to another LI rendering)..
  $('#tree1').tree('loadData', childs, node); // .. and can be cached thereafter
}

function createChild(id) {
  var sym = symsMap[id];
  var val = infoForVal(sym.val);
  //eg.log("id: ", id, ", sym: ", sym);
  if (createForContextsFlag) {
    var nodeID = id;
  } else if (unusedIDs[id]) {
    delete unusedIDs[id];
    nodeID = id;
  } else {
    nodeID = ++nodeIx;
  }
  return { // constant after rendered once (as long there is no drag'n'drop)
    cacheLiFlag: cacheLIs_Flag,
    id: nodeID,
    ID: id, // for anchor
    val: val,
    hash: sym.valHash, // todo for comparison, because val may be e.g. truncated
    label: ((indirectCtxRefSym(sym) ? "[ref] " : "")
            + sym.prefix + ":" + sym.term
            + (sym.reference ? " → " + sym.target : ""))
  }
}

var createForContextsFlag = true;
var symsURLBase = "/symbols-JSON";
var symsURL = symsURLBase + window.location.search;

var jqxhr = $.getJSON(symsURL, function(data) {
  symsMap = data;
  var allSyms = eg.keys(symsMap).sort(sort_syms);
  var contextSyms =
    allSyms
    .filter(pred_sym_eval2context)
    .filter(create_pred_sym_inContext("MAIN"))
    .filter(function(sid) { // exclude syms not standing for referenced ..
      return ! indirectCtxRef(sid); // .. context in MAIN themself
    })
    .sort(sort_syms);
  //console.log(contextSyms);

  var nodes = contextSyms.map(function(id) {
    var sym = symsMap[id];
    var val = sym.val;
    var inContext =
      allSyms.filter(create_pred_sym_inContext(sym.term))
      .sort(sort_symsInContext);
    
    var res = { // caching possible: ..
      // .. LI will be rendered *after* structure with childs has been built ..
      cacheLiFlag: cacheLIs_Flag, // .. so it can be cached
      id: sym.term, //++ix,
      ID: sym.term,
      folderFlag: true,
      label: "[ctx] " + sym.term,
      children: inContext.map(createChild)
    };
    return res;
  });

  $('#tree1').tree({
    data: nodes,
    onCreateLi: function(node, $li) {
      //eg.log("onCreateLi: ", node);
      var titleElem = $li.find('.jqtree-title');
      titleElem.addClass('inspector');
      // add anchor
      if (node.ID) {
        titleElem.before('<a name="' + node.ID + '"></a>');
      }
      if (node.folderFlag) {
        $li.addClass("inspector folder");
      } else {
        //eg.log("node.id: ", node.id, ", node.ID: ", node.ID);
        $li.addClass(classesForVal(symsMap[node.ID].val));
        titleElem.after('<span class="inspector valChar">' + '-></span>'
                        + '<span class="val">'
                        + node.val
                        + '</span>');
      }
      //node.anchor && $li.find('.jqtree-title').before('<a name="' + node.anchor + '"></a>');
      // for more:
      //   $li.attr('id', node.id);
      //   $li.find('.jqtree-title').before('<a name="bar"></a>');
    },
    dragAndDrop: false // false allows selection of text, and caching of LIs
  });
})
  .done(function() {
    // from here on all symname IDs are exhausted, but jqtree needs unique ones:
    createForContextsFlag = false; // this flag states this
    //eg.log( ".done" );
    $('#tree1').bind(
      'tree.dblclick',
      function(event) {
        //console.log(event.node);
        var ID = event.node.ID;
        if (isListSym(ID)) {
          var symsURL = symsURLFor(ID);
          $.getJSON(symsURL, function(data) {
            //console.log(data);
            createFolder(ID, data);
          });
        }
      });
    $(document).bind('keydown.inspector', $.proxy(function( event ) {
      //console.log(event, event.which);
      // from tree.jquery.js
      if ($(document.activeElement).is('textarea,input,select')) {
        return true;
      }
      var node = $('#tree1').tree('getSelectedNode');
      if (! node || node.folderFlag) {
        return true;
      }
      if (event.which === 13) { // Return
        event.preventDefault();
        var ID = node.ID;
        if (isListSym(ID)) {
          var symsURL = symsURLFor(ID);
          $.getJSON(symsURL, function(data) {
            createFolder(ID, data);
          });
        }
      }
    }));
    // for reaching node given by hash:
    $(window).trigger('hashchange');
  })
  .done(checkForPingPong())
  .fail(function(data) {
    var urlVars = eg.getURLVars();
    $('#tree1').after("<h1>Problem</h1>\n"
                      + "<p>" + data.responseText + "</p>");
    console.log( ".fail" );
  })
  .always(function() {
    //console.log( ".always" );
  });
// open its parent folder, if child node, and scroll to it
$(window).on('hashchange', function(ev) {
  var node = $('#tree1').tree('getNodeById', location.hash.substring(1));
  if (! node) { return; }
  var tree = $('#tree1');
  var change;
  for (var n = node.parent; n; n = n.parent) {
    if (n.folderFlag) {
      tree.tree('openNode', n);
      change = true;
    };
  }
  // timeout helps to reach the correct position
  setTimeout(function() { $('#tree1').tree('scrollToNode', node); }, 100);
});

if (null) {
$.getJSON(
    '/symbols-JSON',
    function(data) {
      //alert("hier");
      //console.log(data);
      return;
        $('#tree1').tree({
            data: data
        });
    }
);
}

var pollTimeout = 2000; // 1s
function checkForPingPong() {
  if (eg.getURLVars().pingPong) {
    setTimeout(pollAndUpdateTopFolders, pollTimeout);
  }
}
function heartbeatThen(todoFun) {
  eg.log("heartbeatThen()");
  var jqxhr = $.ajax({
    url: "/heartbeat"
  })
    .done(function(response) {
      eg.log("response: ", response);
      todoFun();
    })
    .fail(function() {
      setTimeout(function() { heartbeatThen(todoFun); }, pollTimeout);
    });
}
function leaveThen(thenFun) {
  eg.log("leave()");
  var jqxhr = $.ajax({
    url: "/leave"
  })
    .done(function(response) {
      eg.log("response: ", response);
    })
    .fail(function() {
      eg.log("fail leave()");
    })
    .always(thenFun);
}
function getNode(node_id) {
  return $('#tree1').tree('getNodeById', node_id);
}
function getState(node_id) {
  return $('#tree1').tree('getState');
}
function getID(node_id) {
  return getNode(node_id).ID;
}
function updateNode(node) {
  var child = createChild(node.ID);
  if (node.val !== child.val) {
    node.cachedLi = false;
    $('#tree1').tree('updateNode', node, child);
  }
}
function symsURLFor(ID) {
  return (symsURLBase + "?inList=" + ID);
}
function updateTopFolder(node_id) {
  var node = getNode(node_id);
  node.cacheLiFlag = false;
  node.children.map(function(childNode) {
    updateNode(childNode);
  });
}
var pingPongFlag = false;
function pollAndUpdateTopFolders() {
  if (! topFolders.length) {
    setTimeout(pollAndUpdateTopFolders, pollTimeout);
    return;
  }
  var openNodes = getState().open_nodes;
  var closedTopFolders = topFolders.filter(function(topFolder_id) {
    return ! eg.arrContains(openNodes, topFolder_id);
  });
  if (closedTopFolders.length) {
    if (pingPongFlag) {
      pingPongFlag = false;
      alert("Some top folders closed:\nstopping ping-pong mode.");
    }
    setTimeout(pollAndUpdateTopFolders, pollTimeout);
    return;
  }
  if (! pingPongFlag) {
    pingPongFlag = true;
    alert("All top folders open:\nstarting ping-pong mode.");
  }
  // all top folders are open from here
  heartbeatThen(function() {
    var actionCount = 0;
    topFolders.map(function(node_id) {
      var ID = getID(node_id);
      $.getJSON(symsURLFor(ID), function(data) {
        copyIntoSymsMap(data);
        updateTopFolder(node_id, data);
      })
        .always(function() {
          if (++actionCount === topFolders.length) {
            leaveThen(function() {
              setTimeout(pollAndUpdateTopFolders, pollTimeout); // repeat
            })
          }
        });
    });
  });
}
