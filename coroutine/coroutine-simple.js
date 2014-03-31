/**
 * A simplified coroutine API intended to interact with the event loop.
 */

// May be able to make use of the Promise API here.  Basically, the main
// thread acts as a coordinator.  When the coroutine yields a promise is
// created that takes care of the callback.

const Coroutine = 
{
    current_: null,

    create: 
    function (thunk) {
	var co = Coroutine_make((v) => thunk());
        this.current_ = co;
        Coroutine_yieldTo(co, null);
        ...;
    },

    surrenderTimeslice: 
    function () {
        if (timesliceExpired()) Coroutine_yieldTo(null, this.current_);
    },

    waitForNotification:
    function () {
	
    }
};
