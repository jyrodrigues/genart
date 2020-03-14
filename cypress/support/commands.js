// ***********************************************
// This example commands.js shows you how to
// create various custom commands and overwrite
// existing commands.
//
// For more comprehensive examples of custom
// commands please read more here:
// https://on.cypress.io/custom-commands
// ***********************************************
//
//
// -- This is a parent command --
// Cypress.Commands.add("login", (email, password) => { ... })
//
//
// -- This is a child command --
// Cypress.Commands.add("drag", { prevSubject: 'element'}, (subject, options) => { ... })
//
//
// -- This is a dual command --
// Cypress.Commands.add("dismiss", { prevSubject: 'optional'}, (subject, options) => { ... })
//
//
// -- This will overwrite an existing command --
// Cypress.Commands.overwrite("visit", (originalFn, url, options) => { ... })
//

// How to set localstorage before tests (to be used with localstorage versioning tests and visual diff
//
//
// https://stackoverflow.com/questions/50820732/in-cypress-set-a-token-in-localstorage-before-test

Cypress.Commands.add('loginWithJWT', () => {
  cy.request({
    method: 'POST',
    url: 'http://localhost:3000/api/users/login',
    body: {
      user: {
        email: 'jake@jake.jake',
        password: 'jakejake',
      }
    }
  })
  .then((resp) => {
    window.localStorage.setItem('jwt', resp.body.user.token)
  })

})
