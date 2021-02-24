$(function() {
    //if(window.location.hostname !== 'localhost'){

    // dont track any elements with data-ga-event="false"
    var exclude = '[data-ga-event="false"]';

    // strips out any text form elements with .visuallyhidden
    function striptext(element){
        // Stop PII data of Agent Name being sent
        if($(element).attr('id') === 'linkConfirmDecline'){
            return 'I do not want to appoint this agent';
        } else {
            // Remove visually hidden text content then return the remaining text
            return element.clone().children('.visuallyhidden').remove().end().text();
        }
    }


    function dataLayerOpenClose(_this){
        if($(_this).closest('details').attr('open')){
            return 'hide'
        } else {
            return 'expand'
        }
    }

    // only take the first part of titles
    var title = (function() {
        var s = $('title').text();
        s = s.substring(0, s.indexOf(' - ')).replace(/:/g, ' -').replace(/\r?\n|\r/g, '');
        return s;
    })();


    // details summary
    $('details summary .summary:not('+exclude+')').each(function(){
        $(this).click(function(e){
            dataLayer.push({
                'event': 'custom_agents_request',
                'agents_event_category': 'accordion - ' +dataLayerOpenClose(this),
                'agents_event_action': title,
                'agents_event_label': striptext($(this))
            });
        });
    });


    //}
});