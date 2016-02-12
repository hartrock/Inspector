var Inspector = Inspector || {};

(function(is) {
  var eg = EvolGo;
  var develMode = true;
  var useInteractFlag = true;
  var debugFlag = true;
  var debugWS_flag = false;
  var debugWS_send_flag = debugWS_flag;
  var debugWS_received_flag = debugWS_flag;
  var debugSTO_flag = false;
  var debugSTR_flag = false;
  var debugGUI_flag = false;
  var debugProtocol_flag = false;
  var dbg_editor_flag = false;
  var hasSymbolsViewFlag;
  var inEvalConsole = false;
  var inEvalIntrospectionCount = 0;
  var inControl = false;
  var inRemoteStartup = false;

  var websocket;
  
  function performEvaluation(command) {
    //console.log("command: " + command);
    //console.log('$(":focus"): ', $(":focus"));
    var jsonObj = { type: "remoteEvalConsole",
                    requestID: "" + ++requestCount,
                    remoteEvalConsole: {
                      ID: "" + ++remoteEvalConsoleCount,
                      input: command
                      //resultTransfer: "bigChunk"
                    }
                  };
    var jsonStr = JSON.stringify(jsonObj);
    lockConsole();
    evalErrorFlag = false;
    inEvalConsole = true;
    doSend(jsonStr);
    log_info(jsonObj.type + " started");
  }
  function introspectionObj(type, symsExpression, callback, multiFlagOrNil) {
    var res
      = { type: "remoteEvalIntrospection",
          requestID: "" + ++requestCount,
          remoteEvalIntrospection: {
            type: type,
            ID: "" + ++remoteEvalIntrospectionCount,
            symsExpression: symsExpression
            // automatic: resultTransfer: "bigChunk"
          }
        };
    multiFlagOrNil !== undefined && (res.multiFlagOrNil = multiFlagOrNil);
    return res;
  }
  function performIntrospection_symbolsMap(symsExpression, callback,
                                           multiFlagOrNil) {
    //console.log("symsExpression: " + symsExpression);
    introspectionCallback = callback;
    var jsonObj = introspectionObj("symbolsMap",
                                   symsExpression, callback, multiFlagOrNil);
    var jsonStr = JSON.stringify(jsonObj);
    evalErrorFlag = false;
    if (! inEvalIntrospectionCount) {
      lockConsole();
    }
    ++inEvalIntrospectionCount;
    doSend(jsonStr);
    log_info(jsonObj.type + " started");
  }
  function h_performIntrospection_symbolsInLists(type, listSymsExpr, callback) {
    //console.log("symsExpression: " + symsExpression);
    introspectionCallback = callback;
    var jsonObj = introspectionObj(type,
                                   listSymsExpr, callback);
    var jsonStr = JSON.stringify(jsonObj);
    evalErrorFlag = false;
    if (! inEvalIntrospectionCount) {
      lockConsole();
    }
    ++inEvalIntrospectionCount;
    doSend(jsonStr);
    log_info(jsonObj.type + ": started");
  }
  function performIntrospection_symbolsInLists(listSymsExpr, callback) {
    return h_performIntrospection_symbolsInLists("symbolsInLists",
                                                 listSymsExpr, callback);
  }
  function performIntrospection_symbolsInListsQuoted(listSymsExpr, callback) {
    return h_performIntrospection_symbolsInLists("symbolsInListsQuoted",
                                                 listSymsExpr, callback);
  }
  function controlObj(controlProps) {
    controlProps.ID = "" + ++remoteControlCount;
    var res = { type: "remoteControl",
                requestID: "" + ++requestCount,
                remoteControl: controlProps
              };
    return res;
  }
  function performControl(controlProps) {
    var jsonObj = controlObj(controlProps);
    //console.debug(jsonObj);
    var jsonStr = JSON.stringify(jsonObj);
    lockConsole();
    controlErrorFlag = false;
    inControl = true;
    doSend(jsonStr);
    log_info(jsonObj.type + " " + JSON.stringify(controlProps));
  }
  var startRemoteCount = 0;
  function performControl_startRemote(argumentStr) {
    ++startRemoteCount;
    //console.log("startRemoteCount: " + startRemoteCount);
    performControl({
      type: "start",
      arguments: argumentStr || ""
    });
  }
  is.performControl_startRemote = performControl_startRemote; // devel export
  function performControl_killRemote(signal) {
    console.log("killRemote: " + signal);
    performControl({
      type: "kill",
      signal: signal
    });
  }
  is.performControl_killRemote = performControl_killRemote; // devel export

  var wsURI = "ws://" + location.host + "/console";
  var console_editor, log_editor, err_editor;
  var console_editor_hadFocus = [];
  var ce, le, ee; // shortcuts
  var curcur;
  var requestCount = 0;
  var remoteEvalConsoleCount = 0;
  var remoteEvalIntrospectionCount = 0;
  var remoteControlCount = 0;
  var singleHist = [""], singleHistIx = 1;
  var multiHist = ["\n"], multiHistIx = 1;
  var promptTextmark, multiEndBookmark;
  var inputMode = "single";
  var multiStartPos;
  var multiEndBookmark;

  var responseStatus = null;
  //
  var evalErrorFlag = false; //
  var evalFinishedFlag = false; // after remote startup it should be true
  //
  var evalConsoleStatus = null;
  var debugPromptFlag = false;
  var interruptPromptFlag = false;
  //
  var evalIntrospectionStatus = null;
  var introspectionCallback;
  var is_node_ids;
  //
  var controlErrorFlag;
  //
  var remote_pid;
  
  // set in init()
  var setWidthHeight; // func
  var topFrac; // changed by resizing console
  var heightOutsideSplit;
  var leftFrac; // in case of two columns; changed by resizing console

  function setWidthHeightOneColumn(a_width, a_height) {
    //console.log(a_width, a_height);
    var ce_width = a_width;
    var ce_height = a_height * topFrac;
    //console.log("ce width: " + width, "height: " + height);
    ce.setSize(ce_width, ce_height);
    var le_width = a_width;
    var le_height = a_height * (1 - topFrac);
    //console.log("le width: " + width, "height: " + height);
    le.setSize(le_width, le_height);
    //console.log($("#sep_tr").height(), a_height, (a_height * 0.5));
  }
  function setWidthHeightTwoColumns(a_width, a_height) {
    //console.log(a_width, a_height);
    var ce_width = a_width * leftFrac;
    var ce_height = a_height * topFrac;
    //console.log("ce width: " + width, "height: " + height);
    ce.setSize(ce_width, ce_height);
    ce.dims = { width: ce_width, height: ce_height };
    var le_width = a_width * (1 - leftFrac);
    var le_height = a_height * (1 - topFrac);
    le.dims = { width: le_width, height: le_height };
    //console.log("le width: " + width, "height: " + height);
    le.setSize(le_width, le_height);
    ee.setSize(ce_width, le_height);
    //console.log($("#sep_tr").height(), a_height, (a_height * 0.5));
  }
  var layoutInfoCount = 0;
  function layoutInfo() {
    log_dbgGUI({
      messages: [
        ++layoutInfoCount,
        "window", "" + [$(window).width(),  $(window).height()],
        "html",   "" + [$("html").width(),  $("html").height()],
        "body",   "" + [$("body").width(),  $("body").height()],
        "table",  "" + [$("table").width(), $("table").height()]
      ]});
  }
  function adaptEditorSizes() {
    $("#div_symbolsView").hide();
    $("body").hide();
    var win_width_withoutScrollbar = $(window).width();
    var win_height_withoutScrollbar = $(window).height();
    //console.log("$(window).width(): " + $(window).width(),
    //            "$(window).height(): " + $(window).height());
    $("body").show();

    // roughly guessed available width/height
    var a_width = win_width_withoutScrollbar;
    var a_height = win_height_withoutScrollbar - heightOutsideSplit;
    setWidthHeight(a_width, a_height);

    // remove horizontal scrollbar in window
    while ($(window).height() < win_height_withoutScrollbar) {
      a_width -= 1;
      setWidthHeight(a_width, a_height);
    }
    layoutInfo();
    // remove vertical scrollbar in window
    while ($(window).width() < win_width_withoutScrollbar) {
      a_height -= 1;
      setWidthHeight(a_width, a_height);
    }
    layoutInfo();

    // fill area of lastly removed scrollbar
    while ($(window).height() === win_height_withoutScrollbar) {
      a_width += 1;
      setWidthHeight(a_width, a_height);
    }
    --a_width;
    setWidthHeight(a_width, a_height);
    while ($(window).width() === win_width_withoutScrollbar) {
      a_height += 1;
      setWidthHeight(a_width, a_height);
    }
    --a_height;
    setWidthHeight(a_width, a_height);

    // adapt symbol tree
    var leftHeight = $("#wrapper_console").height();
    $("#div_symbolsView").height(leftHeight);
    var bottomWidth = $("#wrapper_log").width();
    $("#div_symbolsView").width(bottomWidth);
    $("#div_symbolsView").show();
  }

  function init_log()
  {
    var output = document.getElementById("log");
    log_editor = CodeMirror.fromTextArea(output, {
      lineNumbers: true,
      mode: "text",
      lineWrapping: true
      //viewportMargin: 100, //"Infinity",
      //scrollbarStyle: null
    });
    le = log_editor;
    is.le = le; // export
  }
  function init_stderr()
  {
    var output = document.getElementById("stderr");
    err_editor = CodeMirror.fromTextArea(output, {
      lineNumbers: true,
      mode: "text",
      scrollbarStyle: null
    });
    ee = err_editor;
    is.ee = ee; // export
  }
  function getLine() {
    var cursor = console_editor.getCursor();
    return console_editor.getLine(cursor.line);
  }
  function atEOL() {
    var cursor = console_editor.getCursor();
    return getLine().length === cursor.ch;
  }
  function atEmptyLine() {
    return getLine().length === 0;
  }
  function promptLineOrNil() {
    var fpt = promptTextmark && promptTextmark.find();
    //console.log("fpt: ", fpt);
    return fpt && fpt.from.line;
  }
  function inSingleCommandLine() {
    var cursor = console_editor.getCursor();
    //return cursor.line == curcur.line && cursor.ch != 0;
    return cursor.line === promptLineOrNil() && cursor.ch !== 0;
  }
  function inMultiCommandLine() {
    var cursor = console_editor.getCursor();
    var promptLine = promptLineOrNil();
    return cursor.line === (promptLine && promptLine + 1);
  }
  function getCommandLine() {
    //return console_editor.getLine(curcur.line);
    return console_editor.getLine(promptLineOrNil());
  }
  function getSingleCommand() {
    var lineWithoutPrompt = getCommandLine().substr(curcur.ch);
    return lineWithoutPrompt;
  }
  function getMultiCommand() {
    var curPos = ce.getCursor(); // should be at ch:0
    var sel;
    var promptLine = promptLineOrNil();
    if (promptLine && curPos.line > promptLine) {
      ce.setSelection(posEOLCommandLine(), curPos);
      sel = ce.getSelection();
      ce.setSelection(curPos);
    } else {
      sel = "\n";
    }
    return sel;
  }
  function atBeginOfMultiCommandLine() {
    var curPos = ce.getCursor(); // should be at ch:0
    var promptLine = promptLineOrNil();
    return promptLine && curPos.line === promptLine + 1 && curPos.ch === 0;
  }
  function multiEndPosOrNull() {
    return multiEndBookmark && multiEndBookmark.find();
  }
  function EOF() {
    return (ce.doc.indexFromPos(ce.getCursor())
            === ce.getDoc().getValue().length);
  }
  is.EOF = EOF;
  function getMultiCommandReadline() {
    var beginPos, beginReadlinePos, endPos, multiCommand;
    beginPos = posEOLCommandLine();
    beginReadlinePos = { line: beginPos.line + 1, ch: 0 };
    endPos = multiEndPosOrNull()
      || { line: beginReadlinePos.line + 1, ch: 0 }; // surrogate for non-existing end bookmark
    ce.setSelection(beginPos, endPos);
    multiCommand = ce.getSelection();
    ce.setSelection(beginReadlinePos);
    return multiCommand;
  }
  function posEOLCommandLine() {
    var line = promptLineOrNil();
    var lineStr = console_editor.getLine(line);
    //console.log("line: ", line, ", lineStr: ", lineStr);
    return { line: line, ch: lineStr.length };
  }
  function posBOL() {
    var cursor = console_editor.getCursor();
    return { line: cursor.line, ch: 0 };
  }
  function posEOL() {
    var cursor = console_editor.getCursor();
    var lineStr = console_editor.getLine(cursor.line);
    return { line: cursor.line, ch: lineStr.length };
  }
  function updatePrompt() {
    if (promptTextmark) {
      promptTextmark.clear();
    }
    promptTextmark =
      console_editor.doc.markText({ line: curcur.line, ch: 0 }, curcur,
                                  { className: (evalErrorFlag
                                                ? "promptError"
                                                : (evalFinishedFlag
                                                   ? "promptSingle"
                                                   : "promptUnfinished")),
                                    atomic: true});
/*
    promptBookmark =
      console_editor.doc.setBookmark(curcur);
    console.log("curcur: ", curcur, ", promptBookmark: ", promptBookmark,
                ", .find: ", promptBookmark.find());
    console.log("curcur: ", curcur, ", promptTextmark: ", promptTextmark,
                ", .find: ", promptTextmark.find());
*/
/*
    if (curcur) {
      //console_editor.doc.setCursor(curcur);
    }
*/
  }
  function multiStart() {
    inputMode = "multi";
    var curPos = console_editor.getCursor();
    var multiEndPos = { line: curPos.line + 2, ch: 0 };
    multiEndBookmark && multiEndBookmark.clear();
    /*
    startBookmark = console_editor.doc.setBookmark(multiStartPos);
    var multiStartPosBOL = { line: multiStartPos.line, ch: 0 };
    console.log("startBookmark: ", startBookmark);
    console_editor.doc.markText(multiStartPosBOL, multiStartPos);
    */
  }
  function multiStop() {
    inputMode = "single";
    multiEndBookmark && multiEndBookmark.clear();
  }
  function inEmptyCommandLine() {
    return inSingleCommandLine() && getSingleCommand() == "";
  }
  function inEmptyLine() {
    return getLine() == "";
  }

  function handle_UpDown_single(cm, mode) {
    var changeCommand = false;
    singleHist[singleHistIx] = getSingleCommand();
    if (mode === "Up" && singleHistIx > 0) {
      --singleHistIx;
      changeCommand = true;
    }
    if (mode === "Down" && singleHistIx < singleHist.length - 1) {
      ++singleHistIx;
      changeCommand = true;
    }
    if (changeCommand) {
      cm.replaceRange(singleHist[singleHistIx],
                      { line: promptLineOrNil(), ch: curcur.ch },
                      posEOLCommandLine());
    }
  }
  function handle_UpDown_multi(cm, mode) {
    var multiCommand = getMultiCommandReadline();
    var changeCommand = false;
    if (! multiCommand) {
      log_warn("Cannot find end marker of multilines.");
      return CodeMirror.Pass;
    }
    multiHist[multiHistIx] = multiCommand;
    if (mode === "Up" && multiHistIx > 0) {
      --multiHistIx;
      changeCommand = true;
    }
    if (mode === "Down" && multiHistIx < multiHist.length - 1) {
      ++multiHistIx;
      changeCommand = true;
    }
    if (changeCommand) {
      var endPos = multiEndPosOrNull();
      var multiHistCommand = multiHist[multiHistIx];
      var beginPos = posEOLCommandLine();
      var readlineBeginPos = { line: beginPos.line + 1, ch: 0 };
      if (! endPos) {
        endPos = (EOF()
                  ? { line: readlineBeginPos.line + 1 }
                  : readlineBeginPos);
      }
      if (dbg_editor_flag) {
        console.log("multiHistCommand: ", multiHistCommand, multiHist);
        console.log("1. promptLineOrNil(): ", promptLineOrNil(),
                    ", beginPos: ", beginPos,
                    ", endPos: ", endPos);
      }
      cm.replaceRange(multiHistCommand,
                      beginPos,
                      endPos);
      if (dbg_editor_flag) {
        console.log("2. promptLineOrNil(): ", promptLineOrNil(),
                    ", beginPos: ", beginPos,
                    ", endPos: ", endPos);
      }
      cm.setCursor(readlineBeginPos);
      multiEndBookmark && multiEndBookmark.clear();
      var newEndPos = cm.posFromIndex(cm.indexFromPos(beginPos)
                                      + multiHistCommand.length);
      multiEndBookmark =
        cm.setBookmark(newEndPos);
      cm.scrollIntoView(newEndPos);
      dbg_editor_flag
        && console.log("multiEndBookmark.find(): ", multiEndBookmark.find());
    }
  }
  function inCommandLine() {
    return (inputMode === "single" && inSingleCommandLine()
            || inputMode === "multi" && inMultiCommandLine());
  }
  function handle_UpDown(cm, mode) {
    if (inputMode === "single") {
      return handle_UpDown_single(cm, mode);
    } else { // multi
      return handle_UpDown_multi(cm, mode);
    }
  }
  function init_console()
  {
    var textarea = document.getElementById("console");
    console_editor = CodeMirror.fromTextArea(textarea, {
      lineNumbers: true,
      //mode: "scheme",
      scrollbarStyle: null,
      //viewportMargin: Infinity,
      mode: "text",
      extraKeys: {
        // http://stackoverflow.com/questions/13469605/detecting-new-line-in-codemirror
        Enter: function(cm, obj){
/*
          console.log("Enter promptBookmark: ", promptBookmark,
                      ", .find: ", promptBookmark.find());
          console.log("Enter promptTextmark: ", promptTextmark,
                      ", .find: ", promptTextmark.find());
          if (promptTextmark.find()) {
            console.log(", .find.from: ", promptTextmark.find().from);
          }
*/
          var command = null;
          if (inputMode === "single") {
            if (inEmptyCommandLine()
                && ! (debugPromptFlag            // no multiline mode, if ..
                      || interruptPromptFlag)) { // .. debug or interrupt prompt
              multiStart();
            } else {
              var doCommand = inSingleCommandLine() || atEOL() || atEmptyLine();
              if (doCommand) {
                if (inSingleCommandLine()) {
                  cm.setCursor(posEOLCommandLine());
                } else if (atEOL()) {
                  curcur = posBOL();
                }
                command =
                  inSingleCommandLine() ? getSingleCommand() : getLine();
                singleHist[singleHist.length - 1] = command;
                singleHistIx = singleHist.length;
                singleHist.push(""); // corresponding to singleHistIx
              }
            }
          } else { // "multi"
            if (inEmptyLine() || atBeginOfMultiCommandLine()) {
              if (atBeginOfMultiCommandLine()) {
                var endPos =  multiEndPosOrNull();
                if (endPos) {
                  cm.setCursor(endPos);
                }
                //command = getMultiCommandReadline();
              }
              command = getMultiCommand();
              dbg_editor_flag && console.log("command: ", command);
              multiHist[multiHist.length - 1] = command;
              multiHistIx = multiHist.length;
              multiHist.push("\n"); // corresponding to multiHistIx
              multiStop();
            } else {
              // nothing to do
            }
          }
          if (command !== null) {
            if (command && command[0] === "\n") { // multiline mode
              Inspector.performEvaluation("[cmd]\n" + command + "\n[/cmd]\n");
            } else { // single line mode
              Inspector.performEvaluation(command + "\n");
            }
          }
          cm.execCommand("newlineAndIndent");
          if (command !== null) {
            curcur = cm.doc.getCursor(); // after NL
            //console.log("Enter curcur: ", curcur);
          }
        },
        Up: function(cm, obj) {
          if (! inCommandLine()) {
            return CodeMirror.Pass;
          }
          return handle_UpDown(cm, "Up");
        },
        Down: function(cm, obj) {
          if (! inCommandLine()) {
            return CodeMirror.Pass;
          }
          return handle_UpDown(cm, "Down");
        }
      }
    });
    ce = console_editor;
    curcur = CodeMirror.Pos(console_editor.lastLine());
    is.ce = ce; // export
  }

  function onOpen(evt)
  {
    log_info("CONNECTED");
    doSend(JSON.stringify({ type: "test websocket connection",
                            requestID: "" + ++requestCount }));
    // triggers an ERROR:
    //   doSend("check raw...");
  }

  function onClose(evt)
  {
    log_err("Websocket connection to webserver disconnected; code: "
            + evt.code + ".");
  }

  //todo: try class
  // Brute force approach for wait cursor while computing changed layout.
  function waitCursorStart() {
    $("*").css("cursor", "wait");
    // $("html").css("cursor", "wait");
  }
  function waitCursorStop() {
    $("*").css("cursor", "");
    // $("html").css("cursor", "");
  }
  
  function lockConsole() {
    document.querySelector('#wrapper_console').classList.add('waitCursor');
    console_editor_hadFocus.push(console_editor.hasFocus());
    console_editor.setOption("readOnly", "nocursor");
  }
  function unlockConsole(cleanFlag) {
    document.querySelector('#wrapper_console').classList.remove('waitCursor');
    console_editor.setOption("readOnly", false);
    console_editor_hadFocus.pop() && console_editor.focus();
    if (cleanFlag) {
      console_editor_hadFocus = [];
    }
  }
  function assert(val) {
    if (! val) {
      if (console.assert) {
        console.assert(val);
      } else if (console.error) {
        console.error("assert failed");
      }
      log_err("assert failed!");
      return false;
    }
    return true;
  }
  function writeRemoteResults(res) {
    if (res.stdout) {
      writeToConsole(res.stdout);
    }
    if (res.stderr) {
      writeToErr(res.stderr);
    }
  }
  function nL_quote_string (str) {
    return '"' + str + '"';
  }
  function nL_symbolsInListExpression(ID) {
    return "(Introspection:symbols-in-symStr " + nL_quote_string(ID) + ")";
  }
  function nL_symbolsInLists_expression(IDs) {
    var symsStr = "'(";
    var symsStrsArr = IDs.map(nL_quote_string);
    symsStr += symsStrsArr.join(" ");
    symsStr += ")";
    console.log(symsStr);
    return ("(Introspection:symbols-in-symStrs "
            + symsStr
            + ")");
    return;
  }
  var firstFillDoneFlag = false;
  function handle_createTopFolder(event, node) {
    event.preventDefault();
    var sid = node.sid;
    if (is.isDynsym(sid)) {
      sid = is.getDynTargetFrom_dsid(sid);
    }
    if (is.isListSym(sid)) {
      //var listsExpr = "'('" + sid + ")";
      var listsExpr = "'(" + is.quote_sid(sid) + ")";
      performIntrospection_symbolsInListsQuoted(listsExpr,function(data) {
        is.createTopFolder(sid, data[sid]);
      });
    }
  }
  function fillSymbolsTree(data) {
    //log_info("[fillSymbolsTree] data['Class:Class']: " + data['Class:Class']);
    //log_info(JSON.stringify(data['Class:Class']));
    //console.log("data['Class:Class']: ", data['Class:Class'],
    //            JSON.stringify(data['Class:Class']));
    var treeSel = '#div_tree';
    var tree = $(treeSel);
    is.init_setSymbolsTreeSelector(treeSel);
    is.createInitialContextFolders(data);
    if (! firstFillDoneFlag) {
      $(treeSel).bind(
        'tree.dblclick',
        function(event) {
          return handle_createTopFolder(event, event.node);
        }
      );
      $(document).bind('keydown.inspector', $.proxy(function( event ) {
        //console.log("$(document.activeElement): ", $(document.activeElement),
        //            "event: ", event, "inEvalConsole: ", inEvalConsole);
        // from tree.jquery.js
        if ($(document.activeElement).is('textarea,input,select')
            || inEvalConsole) {
          return true;
        }
        var node = tree.tree('getSelectedNode');
        if (! node || node.folderFlag) {
          return true;
        }
        if (event.which === 13) { // Return
          return handle_createTopFolder(event, node);
        }
        if (event.which === 37) { // "ArrowLeft" cursor
          if (node.parent) {
            event.preventDefault();
            tree.tree('scrollToNode', node.parent);
            tree.tree('selectNode', node.parent);
          }
        }
      }));
      is.init_honorHash();
      firstFillDoneFlag = true;
    }
    // also for going to correct pos in tree view honoring hash in URL
    $(window).trigger('hashchange');
  }
  function updateSymbolsView() {
    performIntrospection_symbolsMap("(Util:symbols-all)", is.updateSymbolsView);
  }

  function onMessage(evt)
  {
    var json, type, subtype, subprops, status;
    log_dbgWS_received(evt.data);
    try {
      json = JSON.parse(evt.data);
      //console.log("RESPONSE JSON: ", json);
    } catch (err) {
      log_err({
        messages: [
          "[WS msg] " + err,
          "JSON data -> evt in browser console"
        ]});
      console.log(evt);
      return;
    }
    type = json.type;
    subtype = json.subtype;
    subprops = json.subprops;
    status = json.status;
    
    log_dbgProtocol(type + ": " + (status || (type === "event"
                                              ? json.what
                                              : "???")));
    if (status === "OK") {
      log_dbgWS("Response OK.");
    }
    //console.log(json);
    switch (json.type) {
    case "remoteStartupError":
      inRemoteStartup = false;
    case "remoteEvalError":
      log_err("requestID: " + json.requestID + "\n  "
              + json.message);
      inEvalConsole = false;
      unlockConsole(true);
      break;
    case "evalResult":
      var rr = json.evalResult;
      var evalStatus = rr.evalStatus;
      evalFinishedFlag = (evalStatus === "finished");
      //console.log(rr.triggerType);
      switch (rr.triggerType) {
      case "startup": case "remoteControl":
        if (status === "OK") {
          writeRemoteResults(rr);
          if (evalFinishedFlag) {
            console_editor.execCommand("goDocEnd");
            inRemoteStartup = false;
            unlockConsole();
            hasSymbolsViewFlag
              && performIntrospection_symbolsMap("(Util:symbols-all)",
                                                 fillSymbolsTree);
          }
        }
        else {
          log_err("Startup of remote interpreter failed.");
          inRemoteStartup = false;
          unlockConsole(true);
        }
        break;
      case "remoteEvalConsole":
        assert(inEvalConsole);
        var unlockFlag = false;
        evalConsoleStatus = status;
        if (evalConsoleStatus === "OK") {
          writeRemoteResults(rr);
          if (evalFinishedFlag) {
            debugPromptFlag = rr.promptType === "debug";
            interruptPromptFlag = rr.promptType === "interrupt";
            console.log(rr, interruptPromptFlag);
            // introspection possible at normal and debug prompt, but ..
            if (hasSymbolsViewFlag
                && ! interruptPromptFlag) { // .. not after interrupt
              updateSymbolsView(); // performs introspection
            }
            unlockFlag = true;
          }
        } else { // error
          evalErrorFlag = true;
          log_err((json.requestID
                   ? "requestID: " + json.requestID + "\n  "
                   : "")
                  + json.message);
          unlockFlag = true;
        }
        if (unlockFlag) {
          inEvalConsole = false;
          unlockConsole();
        }
        break;
      case "remoteEvalIntrospection":
        assert(inEvalIntrospectionCount);
        evalIntrospectionStatus = status;
        if (evalIntrospectionStatus === "OK") {
          assert(evalStatus === "finished");
          //console.log("rr.stdout:", rr.stdout);
          if (rr.stdout) { // "\n" before prompt
            var len = (rr.stdout instanceof Array
                       ? rr.stdout[rr.stdout.length - 1].split('\n')[0]
                       : rr.stdout.split('\n')[0]);
            log_info("introspection data length: " + len)
            if (! len.match(/^[0-9]+$/)) {
              log_warn({
                messages: ["[(unexpected) introspection stdout]", rr.stdout]
              });
            }
          }
          if (rr.stderr) {
            log_warn({
              messages: ["[(unexpected) introspection stderr]", rr.stderr]
            });
          }
          introspectionCallback && introspectionCallback(rr.introspection);
        } else { // error
          evalErrorFlag = true;
          log_err((json.requestID
                   ? "requestID: " + json.requestID + "\n  "
                   : "")
                  + json.message);
        }
        ;
        if (! --inEvalIntrospectionCount) {
          unlockConsole();
        }
        break;
      default:
        assert("should not been reached" && false);
        break;
      }
      break;
    case "remoteControlResponse":
      assert(inControl);
      remoteControlStatus = status;
      if (remoteControlStatus === "OK") {
        var pid = subprops.pid;
        switch (subtype) {
        case "start":
          remote_pid = pid;
          var remoteCommand = subprops.remoteCommand;
          log_info({
            messages: [
              json.message,
              "Remote started as:\n  " + remoteCommand
            ]});
          break;
        case "kill":
          var signal = subprops.signal;
          log_info("Remote with PID " + pid
                   + " "
                   + (signal === 15
                      ? "'killed' soft"
                      : signal === 9
                      ? "'killed' hard"
                      : signal === 2
                      ? "interrupted"
                      : "")
                   + ": signal " + signal + " sent"
                   + (signal === 15 || signal === 9
                      ? " (there follows an event, if remote really has died)"
                      : "")
                   + ".");
          break;
        default:
          log_err("Protocol error.");
          break;
        }
        unlockConsole();
      } else { // error
        log_err(json.message);
        unlockConsole(true);
      }
      inControl = false;
      break;
    case "controlResponse":
      var logFun = json.status === "OK" ? log_info : log_err;
      logFun((json.requestID ? "requestID: " + json.requestID + "\n  " : "")
             + json.message);
      unlockConsole(true);
      break;
    case "event":
      switch (json.what) {
      case "remote_died":
        var messages =
          (inEvalConsole || inEvalIntrospectionCount || inRemoteStartup
           ? ["Remote died: "
              + (inEvalConsole
                 ? "console"
                 : (inEvalIntrospectionCount
                    ? "introspection"
                    : (inRemoteStartup
                       ? "startup"
                       : "?")))
              + " evaluation finished."]
           : ["Remote died."]);
        'exitCode' in json
          && messages.push("exit code: " + json.exitCode);
        json.termSig // > 0
          && messages.push("term sig: " + json.termSig);
        if (inEvalConsole || inEvalIntrospectionCount || inRemoteStartup) {
          (json.exitCode !== undefined
           ? (json.exitCode === 0
              ? log_info
              : log_warn)
           : log_err)({
            messages: messages
          });
          evalErrorFlag = true;
          inEvalConsole = inRemoteStartup = false;
          inEvalIntrospectionCount = 0;
          updatePrompt();
          unlockConsole(true);
        } else {
          log_info({
            messages: messages
          });
        }
        break;
      default:
        log_info("Event " + json.what + " -> ignored."
                 + (json.message ? "\n" + json.message : ""));
        break;
      }
      break;
    case "requestError":
      log_err("Request error (should not happen):\n" + json.message);
      break;
    default:
      assert("should not been reached" && false);
      break;
    }
    //websocket.close();
  }

  function onError(evt)
  {
    //writeToScreen('<span style="color: red;">ERROR:</span> ' + evt.data);
    console.log(evt);
    log_dbg(evt);
  }

  function doSend(message)
  {
    log_dbgWS_send("SENT: " + message); 
    websocket.send(message);
  }

  function writeToScreen(message)
  {
    if (message instanceof Array) {
      message = message.join();
    }
    var pre = document.createElement("p");
    pre.style.wordWrap = "break-word";
    pre.innerHTML = message;
    output.appendChild(pre);
  }
  function writeToOutput(msg) {
    console.log(msg);
  }

  function writeToLog(message)
  {
    if (message instanceof Array) {
      multiReplace_CM_op(le, message, CodeMirror.Pos(le.lastLine()));
    } else {
      le.replaceRange(message, CodeMirror.Pos(le.lastLine()));
    }
    le.scrollIntoView(le.lastLine());
  }
  function writeToErr(message)
  {
    if (! ee) {
      writeToLog("[remote stderr] " + message);
      return;
    }
    if (message instanceof Array) {
      multiReplace_CM_op(ee, message, CodeMirror.Pos(ee.lastLine()));
    } else {
      ee.replaceRange(message, CodeMirror.Pos(ee.lastLine()));
    }
    ee.scrollIntoView(ee.lastLine());
  }
  function multiReplace(cm, messageStrs, pos) {
    var new_pos = pos;
    messageStrs.forEach(function(str, ix) {
      //cm.replaceRange(str, pos);
      cm.replaceRange(str, pos, pos, "*output");
      //new_pos = cm.posFromIndex(cm.indexFromPos(pos)
      //                          + str.length);
      // cursor pos always at end of replacement? ..
      new_pos = cm.getCursor(); //.. seems to be the same here as the former alt
      if (ix % 2 === 1) { // odd ix refers to quoted *formerly* non-UTF-8 str
        //console.log("ix: ", ix);
	//todo: refac for reuse (also used by symbolsTree.js)
        cm.doc.markText(pos, new_pos, {
          className: "is_non-UTF-8",
          title: "non-UTF-8: shown as decimal quoted byte(s)"
        });
      } else {
        // nothing to do: even ix refers to unquoted UTF-8 str
      }
      pos = new_pos;
    });
    return new_pos;
  }
  //todo: reuse refac
  // Saves a lot of time!
  // https://codemirror.net/doc/manual.html#operation
  function multiReplace_CM_op(cm, messageStrs, pos) {
    return cm.operation(function() {
      return multiReplace(cm, messageStrs, pos);
    });
  }
  function writeToConsole(message)
  {
    //console.log("1. writeToConsole() curcur: ", curcur);
    if (message instanceof Array) {
      curcur = multiReplace_CM_op(ce, message, curcur);
    } else {
      ce.replaceRange(message, curcur);
      curcur = ce.getCursor();
      //curcur = ce.posFromIndex(ce.indexFromPos(curcur) + message.length);
      //console.log("new_pos: ", new_pos, ", cm.getCursor(): ", cm.getCursor());
    }
    //ce.setCursor(curcur);
    ce.scrollIntoView(curcur);
    updatePrompt();
    //console.log("3. writeToConsole()");
  }

  // todo: scrollIntoView() and updatePrompt once
  
  // working with msg or msg obj (consisting of muliple messages)
  function writeToLogPrefixMsg(prefix, msg) {
    if (msg instanceof Object) {
      for (var ix = 0; ix < msg.messages.length; ++ix) {
        writeToLog(prefix + "[" + (ix + 1) + "] ");
        writeToLog(msg.messages[ix]); // arg can be string or string arr
        writeToLog("\n");
      }
    } else {
      writeToLog(prefix + " ");
      writeToLog(msg); // arg can be string or str array
      writeToLog("\n");
    }
  }
  function log_err(msg) {
    writeToLogPrefixMsg("[ERROR]", msg);
  }
  function log_warn(msg) {
    writeToLogPrefixMsg("[Warning]", msg);
  }
  function log_info(msg) {
    writeToLogPrefixMsg("[info]", msg);
  }
  function log_dbg(msg) {
    if (debugFlag) {
      writeToLogPrefixMsg("[debug]", msg);
    }
  }
  function log_dbgSTO(msg) { // symbols tree operations
    if (debugSTO_flag) {
      writeToLogPrefixMsg("[debug STO]", msg);
    }
  }
  function log_dbgSTR(msg) { // symbols tree rendering
    if (debugSTR_flag) {
      writeToLogPrefixMsg("[debug STR]", msg);
    }
  }
  function log_dbgWS(msg) { // WebSocket
    if (debugWS_flag) {
      writeToLogPrefixMsg("[debug WS]", msg);
    }
  }
  function log_dbgWS_send(msg) { // WS
    if (debugWS_send_flag) {
      writeToLogPrefixMsg("[debug WS send]", msg);
    }
  }
  function log_dbgWS_received(msg) { // symbols tree rendering
    if (debugWS_received_flag) {
      writeToLogPrefixMsg("[debug WS received]", msg);
    }
  }
  function log_dbgGUI(msg) { // 
    if (debugGUI_flag) {
      writeToLogPrefixMsg("[debug GUI]", msg);
    }
  }
  function log_dbgProtocol(msg) { // 
    if (debugProtocol_flag) {
      writeToLogPrefixMsg("[debug protocol]", msg);
    }
  }

  function init_websocket(cb) {
    websocket = new WebSocket(wsURI);
    websocket.onopen = function(evt) { onOpen(evt); if (cb) { cb(); } };
    websocket.onclose = function(evt) { onClose(evt) };
    websocket.onmessage = function(evt) { onMessage(evt) };
    websocket.onerror = function(evt) { onError(evt) };
  }
  function init_interact() {
    if (! useInteractFlag) {
      return;
    }
    var start_width, start_height, last_width, last_height;
    $("#wrapper_console .CodeMirror").addClass('resize-drag');
    interact('.resize-drag')
      .draggable({
        onmove: window.dragMoveListener,
      })
      .resizable({
        preserveAspectRatio: false,
        edges: { left: false, right: true, bottom: true, top: false }
      })
      .on('resizestart', function (event) {
        //console.log("start event:", event);
        start_width = event.rect.width;
        start_height = event.rect.height;
        //console.log("resizestart", start_width, start_height);
      })
      .on('resizeend', function (event) {
        //console.log("end event:", event);
        var delta_width = last_width - start_width;
        var delta_height = last_height - start_height;
        var width_both = ce.dims.width + le.dims.width;
        var height_both = ce.dims.height + le.dims.height;
        leftFrac = leftFrac + delta_width / width_both;
        topFrac = topFrac + delta_height / height_both;
        waitCursorStart();
        setTimeout(function() { // needed for seeing wait cursor
          adaptEditorSizes();
          waitCursorStop();
        }, 0);
      })
      .on('resizemove', function (event) {
        //console.log(event, event.rect);
        last_width = event.rect.width;
        last_height = event.rect.height;
        var target = event.target;
        // update the element's style
        target.style.width  = event.rect.width + 'px';
        target.style.height = event.rect.height + 'px';
      });
  }

  function init_actionMenu() {
    $("#actionMenu" ).change(function(ev) {
      var actionStr = $("#actionMenu").val();
      console.log(actionStr);
      ev.preventDefault();
      switch (actionStr) {
      case "start":
        var argsStr = $("#remoteArguments").val();
        performControl_startRemote(argsStr);
        break;
      case "kill soft":
        performControl_killRemote(15);
        break;
      case "kill hard":
        performControl_killRemote(9);
        break;
      case "interrupt":
        performControl_killRemote(2);
        break;
      default:
        log_err("Implementation (should not been reached).");
        break;
      }
      $("#actionMenu").val('remote control ');
    });
  }
  function init_navigationMenu() {
    $("#navigationMenu" ).change(function(ev) {
      var urlFragment = $("#navigationMenu").val();
      console.log(urlFragment);
      ev.preventDefault();
      window.location.href = window.location.origin + "/" + urlFragment;
    });
  }
  function close_websocket() {
    if (websocket) {
      //console.log("websocket.close()..");
      websocket.close();
      //console.log("..websocket.close()");
    }
  }
  function init() {
    topFrac = 0.7;
    if (window.location.pathname === "/consoleAlone.html") {
      setWidthHeight = setWidthHeightOneColumn;
      heightOutsideSplit = 0;
    } else {
      hasSymbolsViewFlag = true;
      leftFrac = 0.43;
      setWidthHeight = setWidthHeightTwoColumns;
      heightOutsideSplit = $("#sep_tr").height(); // change with resizing?
      init_stderr();
    }
    init_log();
    init_console();
    adaptEditorSizes();
    if (hasSymbolsViewFlag) {
      init_interact();
    }
    init_actionMenu();
    init_navigationMenu();
    init_websocket(function() {
      inRemoteStartup = true;
      performControl_startRemote($("#remoteArguments").val());
    });
    // may have advantages in some cases
    $(window).on('beforeunload', close_websocket);
  }
  // exports
  is.performEvaluation = performEvaluation;
  is.performIntrospection_symbolsMap = performIntrospection_symbolsMap;
  is.performIntrospection_symbolsInLists = performIntrospection_symbolsInLists;
  is.performIntrospection_symbolsInListsQuoted = performIntrospection_symbolsInListsQuoted;
  is.assert = assert;
  is.log_err = log_err;
  is.log_warn = log_warn;
  is.log_info = log_info;
  is.log_dbg = log_dbg;
  is.log_dbgSTO = log_dbgSTO;
  is.log_dbgSTR = log_dbgSTR;
  is.close_websocket = close_websocket;

  ////
  $(document).ready(function(){
    //$(window).on('load', function(){
    alert("Good news: Inspector's webserver is running.\n"
          + "\n"
          + "Security warning:\n"
          + "----\n"
          + "This application is capable to "
          + "*** look into and *manipulate* your system ***.\n"
          + "So you should:\n"
          + "- *close* port " + location.port
          + " in use here to the outside by some firewall, to\n"
          + "- *** be *not* accessible from outside ***"
          + " your host or - at least - private network.\n"
          + "You have been warned!\n"
          + "----\n");
    init();
  });

}(Inspector));
// EOF
