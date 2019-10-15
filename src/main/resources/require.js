function require(name) {
    if (name === 'react') { return window.React; } 
    else if (name === 'react-dom') { return window.ReactDOM; } 
    else if (name === 'create-react-class') { return window.createReactClass; }
}
