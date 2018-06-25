$(function() {

    // dont track any elements with data-ga-event="false"
    var exclude = '[data-ga-event="false"]';

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

    // only take the first part of titles
    var title = (function() {
        var s = $('title').text();
        s = s.substring(0, s.indexOf(' - ')).replace(/:/g, ' -').replace(/\r?\n|\r/g, '');
        return s;
    })();

    // links
    $('a:not('+exclude+')').each(function(){
        $(this).click(function(e){
            ga('send', 'event', cat(this), title, $(this).text())
        });
    });

    // submit buttons
    $('button, input[type="submit"]').not(''+exclude+'').each(function(){
        $(this).click(function(e){
            ga('send', 'event', 'button-click', title, $(this).text())
        });
    });

    // details summary
    $('details summary:not('+exclude+')').each(function(){
        $(this).click(function(e){
    		ga('send', 'event', 'disclosure-click-'+disclosureOpenClose(this), title, $(this).text())
    	});
    })

    // radio onclick
    $('fieldset:not('+exclude+') input:radio').each(function(){
        $(this).click(function(e){
     		ga('send', 'event', 'radio-click', title, $(this).closest('fieldset').find('legend').text() + " - " + $(this).val())
     	});
    });

    // checkbox onclick
    $('fieldset:not('+exclude+') input:checkbox').each(function(){
        $(this).click(function(e){
     		ga('send', 'event', 'checkbox-click', title, $(this).closest('fieldset').find('legend').text() + " - " + $(this).val())
     	});
    });

    // on form submit
    $('form').submit(function(){

    	// selected radio on submit
    	$('fieldset:not('+exclude+') input:radio:checked').each(function(){
     		ga('send', 'event', 'radio-selected', title, $(this).closest('fieldset').find('legend').text() + " - " + $(this).val())
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
    			ga('send', 'event', 'checkbox-selected', title, $('[name="' + getName + '"]').closest('fieldset').find('legend').text() + " - " + allVals)
    		}
    	});
    });
});
