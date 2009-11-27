// Copyright 2009, Matthias Andreas Benkard.

var loader;
var autocompleteList;
YUI().use('node', 'node-base', 'io-base', 'io-form', 'io-queue', 'json', function (Y) {
    function registerMakeRevisionAction() {
        var button = Y.one("#make-revision");
        button.on("click", function(e) {
            Y.io("make-new-revision",
                 { 'on': { 'complete': function(id, o, args) {
                             Y.log("Neue Version: " + o.responseText);
                             Y.one("#new-version-label").set("innerText", "Gesichert. Neue Version: " + o.responseText + ".");
                           } } });
        });
    }

    function markAsChanged(input) {
        input.setAttribute("style", "background-color: #faa");
    }

    function markAsRegistered(input) {
        input.setAttribute("style", "background-color: #afa");
    }

    function registerScoreInputChange(event, input, row) {
        var id = row.getAttribute("mulk:id");
        var inputnum = 0;
        var inputSiblings = input.parentNode.childNodes;
        for (; inputnum < inputSiblings.length; inputnum++) {
            if (inputSiblings[inputnum] == input) {
                break;
            }
        };
        Y.io("update-student-score",
             { 'on'  : { 'complete': function(id, o, args) {
                             try { Y.log(o.responseText); Y.JSON.parse(o.responseText); }
                             catch(e) { return null; }
                             markAsRegistered(input);
                         } },
               'data': "id=" + id + "&score-number=" + inputnum + "&score=" + input.value });
    }

    function makeScoreInput(cell, value) {
        var input = document.createElement('input');
        input.setAttribute('type', 'text');
        input.setAttribute('maxlength', '3');
        input.setAttribute('size', '3');
        if (value != undefined) {
            input.value = value;
        }
        cell.appendChild(input);
        Y.on("keyup", function(e) { ensureFreeInput(cell); }, input, Y);
        Y.on("keyup", function(e) { markAsChanged(input); }, input, Y);
        Y.on("change", registerScoreInputChange, input, Y, input, cell.parentNode);
        return input;
    };

    function ensureFreeInput(scoreCell) {
        if (!(scoreCell.lastChild.value == undefined || scoreCell.lastChild.value == "")) {
            return makeScoreInput(scoreCell);
        } else {
            return scoreCell.lastChild;
        }
    };

    function updateStudentRowFromName(event, nameInput, scoreCell) {
        Y.log(event);
        Y.log(nameInput);
        Y.log(scoreCell);
        function doUpdate(id, o, args) {
            ensureFreeStudentRow();

            var data = o.responseText;
            try {
                var student = Y.JSON.parse(data);
            } catch (e) {
                return null;
            };
            Y.log(student);

            scoreCell.parentNode.setAttribute("mulk:id", student.id);

            while (scoreCell.firstChild) {
                scoreCell.removeChild(scoreCell.firstChild);
            };

            for (var i = 0; i < student.score.length; i++) {
                var x = student.score[i];
                makeScoreInput(scoreCell, x);
            };

            var freeInput = makeScoreInput(scoreCell);
            freeInput.focus();
        }
        var request = Y.io("find-student", { 'on'  : { 'complete': doUpdate },
                                             'data': "name=" + nameInput.value });
    };

    function makeStudentRow() {
        var table = document.getElementById('ergebnisse');
        var num = table.rows.length;
        var row = table.insertRow(num);

        var cell = row.insertCell(0);
        var input = document.createElement('input');
        var completion = document.createElement('div');
        input.setAttribute('type', 'text');
        completion.setAttribute('class', 'autocomplete');
        cell.appendChild(input);
        cell.appendChild(completion);
        new Autocompleter.Local(input, completion, autocompleteList, { 'fullSearch' : true, 'partialChars': 1 });

        var cell = row.insertCell(1);
        makeScoreInput(cell);

        Y.on("blur", updateStudentRowFromName, input, Y, input, cell);

        return row;
    };

    function ensureFreeStudentRow() {
        var table = document.getElementById('ergebnisse')
        var num = table.rows.length;
        var input = table.rows[num - 1].cells[0].firstChild;
        if (!(input.value == undefined || input.value == "")) {
            return makeStudentRow();
        } else {
            return table.lastChild;
        }
    };

    Y.on('domready', registerMakeRevisionAction);
    Y.on('domready', makeStudentRow);
});
