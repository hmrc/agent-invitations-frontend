$(function() {
    if(window.location.hostname !== 'localhost'){

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

        // is it a button, error, or link?
        function cat(_this){
        	if($(_this).hasClass('button')){
        		return 'button-click'
        	}else if($(_this)[0].hasAttribute("data-focuses") ){
        		return 'link-click-error'
        	}else{
        		return 'link-click'
        	}
        }

        // open or close state
        function disclosureOpenClose(_this){
        	if($(_this).closest('details').attr('open')){
        		return 'close'
        	}else{
        		return 'open'
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

        // links
        $('a:not('+exclude+')').each(function(){
            $(this).click(function(e){
                ga('send', 'event', cat(this), title, striptext($(this)))
            });
        });

        // submit buttons
        $('button, input[type="submit"]').not(''+exclude+'').each(function(){
            $(this).click(function(e){
                ga('send', 'event', 'button-click', title, striptext($(this)))
            });
        });

        // details summary
        $('details summary .summary:not('+exclude+')').each(function(){
            $(this).click(function(e){
        		ga('send', 'event', 'disclosure-click-'+disclosureOpenClose(this), title, striptext($(this)))
                dataLayer.push({
                    'event': 'custom_agents_request',
                    'agents_event_category': 'accordion - ' +dataLayerOpenClose(this),
                    'agents_event_action': title,
                    'agents_event_label': striptext($(this))
                });
        	});
        })

        // on form submit
        $('form').submit(function(){

        	// selected radio on submit
        	$('fieldset:not('+exclude+') input:radio:checked').each(function(){
         		ga('send', 'event', 'radio-selected', title, $(this).closest('fieldset').attr('id') + " - " + $(this).val())
        	});

        	// selected checkbox on submit
        	var getName;

        	$('fieldset:not('+exclude+')').each(function(){
        		var allVals = [];

        		getName = $(this).find('input:checkbox:first').attr('name')

        		$('[name="' + getName + '"]:checked').each(function(){
        			allVals.push($(this).val());
        		});

        		if(getName){
        			ga('send', 'event', 'checkbox-selected', title, $('[name="' + getName + '"]').closest('fieldset').attr('id') + " - " + allVals)
        		}
        	});
        });
    }
});