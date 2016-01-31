(function(is) {

  var eg = EvolGo;
  is.log_err = eg.error;
  is.log_warn = eg.warn;
  is.log_info = eg.info;
  is.log_dbg = is.log_dbgSTO = is.log_dbgSTR = function() {}; //eg.log;
  
  var symsURLBase = "/symbols-JSON";
  var symsURL = symsURLBase + window.location.search;
  var treeSel = '#div_tree';

  var pollTimeout = 2000; // ms
  function checkForPingPong() {
    //console.log("2. done (checkForPingPong)");
    if (eg.getURLVars().pingPong) {
      //console.log("here...");
      is.setPingPongMode();
      setTimeout(pollAndUpdateTopFolders, pollTimeout);
    }
  }
  function heartbeatThen(todoFun) {
    //eg.log("heartbeatThen()");
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
  function symsURLFor(ID) {
    return (symsURLBase + "?inList=" + ID);
  }
  var pingPongFlag = false;
  function pollAndUpdateTopFolders() {
    if (! is.topFolders.length) {
      setTimeout(pollAndUpdateTopFolders, pollTimeout);
      return;
    }
    var openNodes = is.tree_getState().open_nodes;
    var closedTopFolders = is.topFolders.filter(function(topFolder_id) {
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
      is.topFolders.map(function(node_id) {
        var ID = is.node_ID(node_id);
        // $.getJSON(symsURLFor(ID), function(data) {
        //   is.copyIntoSymsMap(data);
        //   is.updateTopFolder(node_id);
        // })
        $.getJSON(symsURLFor(ID),
                  eg.bindThisNArgs(is.updateListFolder, null, node_id))
          .always(function() {
            if (++actionCount === is.topFolders.length) {
              leaveThen(function() {
                setTimeout(pollAndUpdateTopFolders, pollTimeout); // repeat
              })
            }
          });
      });
    });
  }

  var jqxhr = $.getJSON(symsURL, function(data) {
    is.init_setSymbolsTreeSelector(treeSel);
    is.createInitialContextFolders(data);
  })
    .done(function() {
      //eg.log( ".done" );
      $(treeSel).bind(
        'tree.dblclick',
        function(event) {
          //console.log(event.node);
          var ID = event.node.ID;
          if (is.isListSym(ID)) {
            var symsURL = symsURLFor(ID);
            $.getJSON(symsURL, function(data) {
              //console.log(data);
              is.createTopFolder(ID, eg.keys(data));
            });
          }
        });
      $(document).bind('keydown.inspector', $.proxy(function( event ) {
        //console.log("$(document.activeElement): ", $(document.activeElement),
        //            "event: ", event);
        if ($(document.activeElement).is('textarea,input,select')) {
          return true;
        }
        var node = $(treeSel).tree('getSelectedNode');
        if (! node || node.folderFlag) {
          return true;
        }
        if (event.which === 13) { // Return
          event.preventDefault();
          var ID = node.ID;
          if (is.isListSym(ID)) {
            var symsURL = symsURLFor(ID);
            $.getJSON(symsURL, function(data) {
              is.createTopFolder(ID, eg.keys(data));
            });
          }
        }
      }));
      // for reaching node given by hash:
      $(window).trigger('hashchange');
    })
    .done(checkForPingPong)
    .fail(function(data) {
      var urlVars = eg.getURLVars();
      $(treeSel).after("<h1>Problem</h1>\n"
                       + "<p>" + data.responseText + "</p>");
      console.log( ".fail" );
    })
    .always(function() {
      //console.log( ".always" );
    });

  is.init_honorHash();

}(Inspector))
