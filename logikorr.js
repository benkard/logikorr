var loader;
var autocompleteList;
YUI().use('node-base', 'io-base', 'io-form', 'io-queue', 'json', function (Y) {
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
            var data = o.responseText;
            try {
                var student = Y.JSON.parse(data);
            } catch (e) {
                return null;
            };
            Y.log(student);

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
        new Autocompleter.Local(input, completion, autocompleteList, { 'fullSearch' : true });

        var cell = row.insertCell(1);
        makeScoreInput(cell);

        Y.on("blur", updateStudentRowFromName, input, Y, input, cell);
    };

    return Y.on('domready', makeStudentRow);
});
