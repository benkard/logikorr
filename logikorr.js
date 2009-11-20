var loader;
var autocompleteList;
YUI().use('node-base', 'io-base', 'io-form', 'io-queue', function (Y) {
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
        var input = document.createElement('input');
        input.setAttribute('type', 'text');
        input.setAttribute('maxlength', '3');
        input.setAttribute('size', '3');
        cell.appendChild(input);
    };

    return Y.on('domready', makeStudentRow);
});
