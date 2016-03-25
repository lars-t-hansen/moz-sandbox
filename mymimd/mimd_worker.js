postMessage("I\'m working before postMessage(\'ali\').");

onmessage = function (oEvent) {
    switch (typeof oEvent.data) {
	case "string": postMessage("Hi " + oEvent.data); break;
	case "object": oEvent.data[0] = 42; postMessage("ho"); break;
    }
};
