var hooks = require("hooks");
var fetch = require("node-fetch");

const instances = [
  {
    id: "46a288ae-7475-4cdd-a04c-3826c9a4b5f5",
    name: "test instance",
    host: "http://localhost:33333",
    description: "longer text describing the instance",
    visibility: "user",
  },
  {
    id: "0d8b3c29-88a5-4681-ad7a-17f5689e31f3",
    name: "minimal instance",
    host: "http://127.0.0.1:33333",
    visibility: "user",
  },
];

function createInstance(data) {
  return fetch("http://localhost:33334/api/instances", {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify(data),
  });
}

function destroyInstance({ id }) {
  return fetch(`http://localhost:33334/api/instances/${id}`, {
    method: "DELETE",
  });
}

hooks.beforeEach(function (transactions, done) {
  return Promise.all(instances.map(createInstance)).then((results) => {
    done();
  });
});

hooks.afterAll(function (transactions, done) {
  return Promise.all(instances.map(destroyInstance)).then((results) => {
    done();
  });
});
