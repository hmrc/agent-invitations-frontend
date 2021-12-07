$(document).ready(function() {

    var selectCountryEl = document.querySelector('#country-auto-complete')
    if (selectCountryEl) {
        accessibleAutocomplete.enhanceSelectElement({
            autoselect: true,
            defaultValue: selectCountryEl.options[selectCountryEl.options.selectedIndex].innerHTML,
            minLength: 1,
            selectElement: selectCountryEl
        })
    }

    var selectClientEl = document.querySelector('#client-auto-complete')
    if (selectClientEl) {
        accessibleAutocomplete.enhanceSelectElement({
            autoselect: true,
            defaultValue: selectClientEl.options[selectClientEl.options.selectedIndex].innerHTML,
            minLength: 1,
            selectElement: selectClientEl
        })
    }



    function findCountry(country) {
        return country == $("#country-auto-complete").val();
    }

    function findClient(client) {
        return client == $("#client-auto-complete").val();
    }

    //custom handle for not found countries
    $('#country-auto-complete').change(function () {
        var changedValue = $(this).val()
        var array = [];

        $('.autocomplete__menu li').each(function () {
            array.push($(this).text())
        })

        if (array == "No results found") {
            $('#country-auto-complete-select').append('<option id="notFound" value="NOTFOUND">No results found</option>')
            $('#country-auto-complete-select').val('NOTFOUND').attr("selected", "selected");

        } else if (array == "") {
            $('#country-auto-complete-select').val('').attr("selected", "selected");
        }

    });

    //custom handle for not found clients
    $('#client-auto-complete').change(function () {
        var changedValue = $(this).val()
        var array = [];

        $('.autocomplete__menu li').each(function () {
            array.push($(this).text())
        })

        if (array == "No results found") {
            $('#client-auto-complete-select').append('<option id="notFound" value="NOTFOUND">No results found</option>')
            $('#client-auto-complete-select').val('NOTFOUND').attr("selected", "selected");

        } else if (array == "") {
            $('#client-auto-complete-select').val('').attr("selected", "selected");
        }

    });

    $('a[role=button]').keyup(function(e) {
        // get the target element
        var target = e.target;
        // if the element has a role='button' and the pressed key is a space, we'll simulate a click
        if (e.keyCode === 32) {
            e.preventDefault();
            // trigger the target's click event
            target.click()
        }
    });

});