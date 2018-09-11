'use strict'

// Import index.html so it gets copied to dist
import "./index.html";
import { Elm } from "./Main.elm";

var app = Elm.Main.init({
    node: document.getElementById('elm'),
    flags: JSON.parse(localStorage.getItem('genart/cache')) || []
});

app.ports.cache.subscribe(data => {
    console.log(data);
    let cache = JSON.parse(localStorage.getItem('genart/cache')) || [];
    cache.push(data);
    localStorage.setItem('genart/cache', JSON.stringify(cache));
});