(function() {
    this.leaveEntity = function() { 
        // While leaving the aether disable the hands short
        Script.setTimeout(function() {
            Messages.sendLocalMessage('Hifi-Hand-Disabler', 'both');
            Script.setTimeout(function() {
                Messages.sendLocalMessage('Hifi-Hand-Disabler', 'none');
            }, 500);
        }, 500);
    };
});
