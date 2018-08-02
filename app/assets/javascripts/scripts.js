$(function() {
    //Accessibility
    var errorSummary =  $('#error-summary-display'),
    $input = $('input:text')
    //Error summary focus
    if (errorSummary){ errorSummary.focus() }
    $input.each( function(){
        if($(this).closest('label').hasClass('form-field--error')){
            $(this).attr('aria-invalid', true)
        }else{
            $(this).attr('aria-invalid', false)
        }
    });
    //Trim inputs and Capitalize postode
    $('[type="submit"]').click(function(){
        $input.each( function(){
            if($(this).val() && $(this).attr('name') === 'postcode'){
                $(this).val($(this).val().toUpperCase().replace(/\s\s+/g, ' ').trim())
            }else{
                $(this).val($(this).val().trim())
            }
        });
    });
    //Add aria-hidden to hidden inputs
    $('[type="hidden"]').attr("aria-hidden", true)

    //arrange validation messages/classes to correct pattern

    $('.form-date label.form-field--error').each(function () {

            $(this).closest('div').addClass('form-field--error')
            var $relocate = $(this).closest('fieldset').find('legend')
            $(this).find('.error-notification').appendTo($relocate)

    })

    GOVUK.details.init()
});
