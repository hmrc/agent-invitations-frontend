$(function() {
    function cat(_this){
    	if($(_this).hasClass('button')){
    		return 'button'
    	}else if($(_this)[0].hasAttribute("data-focuses") ){
    		return 'link-errors'
    	}else{
    		return 'link'
    	}
    }
    function disclosureOpenClose(_this){
    	if($(_this).attr('aria-expanded') == 'true'){
    		return ' - close'
    	}else{
    		return ' - open'
    	}
    }
    // links
    $('a:not([data-ga-event="false"]').each(function(){
    	$(this).click(function(e){
     		ga('send', 'event', cat(this), 'click', $(this).text())
     	});
    });

    // buttons
    $('button, input[type="submit"]').not('[data-ga-event="false"]').each(function(){
    	$(this).click(function(e){
     		ga('send', 'event', 'button', 'submit', $(this).text())
     	});
    });

    // details summary
    $('details summary:not([data-ga-event="false"])').each(function(){
    	$(this).click(function(e){
    		ga('send', 'event', 'disclosure', 'click', $(this).text() + disclosureOpenClose(this))
    	});
    })

    // radio onclick
    $('fieldset:not([data-ga-event="false"]) input:radio').each(function(){
    	$(this).click(function(e){
     		ga('send', 'event', 'radio', 'click', $(this).closest('fieldset').find('legend').text() + " - " + $(this).val())
     	});
    });

    // checkbox onclick
    $('fieldset:not([data-ga-event="false"]) input:checkbox').each(function(){
    	$(this).click(function(e){
     		ga('send', 'event', 'checkbox', 'click', $(this).closest('fieldset').find('legend').text() + " - " + $(this).val())
     	});
    });

    // on form submit
    $('form').submit(function(){

    	// selected radio on submit
    	$('fieldset:not([data-ga-event="false"]) input:radio:checked').each(function(){
     		ga('send', 'event', 'radio', 'selected', $(this).closest('fieldset').find('legend').text() + " - " + $(this).val())
    	});

    	// selected checkbox on submit
    	var getName;

    	$('fieldset:not([data-ga-event="false"])').each(function(){
    		var allVals = [];

    		getName = $(this).find('input:checkbox:first').attr('name')

    		$('[name="' + getName + '"]:checked').each(function(){
    			allVals.push($(this).val());
    		});

    		if(getName){
    			ga('send', 'event', 'checkbox', 'selected', $('[name="' + getName + '"]').closest('fieldset').find('legend').text() + " - " + allVals)
    		}
    	});
    });
});
