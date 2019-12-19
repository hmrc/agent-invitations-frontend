if (window.jsConfig && window.jsConfig.timeoutEnabled) {
    GOVUK.sessionTimeout({
        timeout: window.jsConfig.timeout,
        countdown: window.jsConfig.countdown,
        keep_alive_url: window.jsConfig.keep_alive_url,
        message: window.jsConfig.message,
        logout_url: window.jsConfig.logout_url,
        timed_out_url: window.jsConfig.timed_out_url
    })
}