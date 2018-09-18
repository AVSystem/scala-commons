package com.avsystem.commons
package rest.openapi

import com.avsystem.commons.rest.RestTestApi
import com.avsystem.commons.serialization.json.JsonStringOutput
import org.scalatest.FunSuite

class OpenApiGenerationTest extends FunSuite {
  test("openapi for RestTestApi") {
    val openapi = RestTestApi.openapiMetadata.openapi(
      Info("Test API", "0.1", description = "Some test REST API"),
      servers = List(Server("http://localhost"))
    )
    assert(JsonStringOutput.writePretty(openapi) ==
      """{
        |  "openapi": "3.0.1",
        |  "info": {
        |    "title": "Test API",
        |    "version": "0.1",
        |    "description": "Some test REST API"
        |  },
        |  "paths": {
        |    "/prefix/{p0}/subget/{p1}": {
        |      "get": {
        |        "responses": {
        |          "200": {
        |            "content": {
        |              "application/json": {
        |                "schema": {
        |                  "type": "string"
        |                }
        |              }
        |            }
        |          }
        |        },
        |        "operationId": "prefix_get_subget",
        |        "parameters": [
        |          {
        |            "name": "p0",
        |            "in": "path",
        |            "required": true,
        |            "schema": {
        |              "type": "string"
        |            }
        |          },
        |          {
        |            "name": "X-H0",
        |            "in": "header",
        |            "required": true,
        |            "schema": {
        |              "type": "string"
        |            }
        |          },
        |          {
        |            "name": "q0",
        |            "in": "query",
        |            "required": true,
        |            "schema": {
        |              "type": "string"
        |            }
        |          },
        |          {
        |            "name": "p1",
        |            "in": "path",
        |            "required": true,
        |            "schema": {
        |              "type": "integer",
        |              "format": "int32"
        |            }
        |          },
        |          {
        |            "name": "X-H1",
        |            "in": "header",
        |            "required": true,
        |            "schema": {
        |              "type": "integer",
        |              "format": "int32"
        |            }
        |          },
        |          {
        |            "name": "q1",
        |            "in": "query",
        |            "required": true,
        |            "schema": {
        |              "type": "integer",
        |              "format": "int32"
        |            }
        |          }
        |        ]
        |      }
        |    },
        |    "/formPost": {
        |      "post": {
        |        "responses": {
        |          "200": {
        |            "content": {
        |              "application/json": {
        |                "schema": {
        |                  "type": "string"
        |                }
        |              }
        |            }
        |          }
        |        },
        |        "operationId": "post_formPost",
        |        "parameters": [
        |          {
        |            "name": "q1",
        |            "in": "query",
        |            "required": true,
        |            "schema": {
        |              "type": "string"
        |            }
        |          }
        |        ],
        |        "requestBody": {
        |          "content": {
        |            "application/x-www-form-urlencoded": {
        |              "schema": {
        |                "type": "object",
        |                "properties": {
        |                  "p1": {
        |                    "type": "string"
        |                  },
        |                  "p2": {
        |                    "type": "integer",
        |                    "format": "int32"
        |                  }
        |                },
        |                "required": [
        |                  "p1",
        |                  "p2"
        |                ]
        |              }
        |            }
        |          },
        |          "required": true
        |        }
        |      }
        |    },
        |    "/trivialGet": {
        |      "get": {
        |        "responses": {
        |          "204": {}
        |        },
        |        "operationId": "get_trivialGet"
        |      }
        |    },
        |    "/complexParams": {
        |      "post": {
        |        "responses": {
        |          "204": {}
        |        },
        |        "operationId": "post_complexParams",
        |        "requestBody": {
        |          "content": {
        |            "application/json": {
        |              "schema": {
        |                "type": "object",
        |                "properties": {
        |                  "baseEntity": {
        |                    "$ref": "#/components/schemas/BaseEntity"
        |                  },
        |                  "flatBaseEntity": {
        |                    "nullable": true,
        |                    "allOf": [
        |                      {
        |                        "$ref": "#/components/schemas/FlatBaseEntity"
        |                      }
        |                    ]
        |                  }
        |                },
        |                "required": [
        |                  "baseEntity"
        |                ]
        |              }
        |            }
        |          },
        |          "required": true
        |        }
        |      }
        |    },
        |    "/moreFailingGet": {
        |      "get": {
        |        "responses": {
        |          "204": {}
        |        },
        |        "operationId": "get_moreFailingGet"
        |      }
        |    },
        |    "/failingGet": {
        |      "get": {
        |        "responses": {
        |          "204": {}
        |        },
        |        "operationId": "get_failingGet"
        |      }
        |    },
        |    "/multiParamPost/{p1}/p1/{p2}": {
        |      "post": {
        |        "responses": {
        |          "200": {
        |            "content": {
        |              "application/json": {
        |                "schema": {
        |                  "$ref": "#/components/schemas/RestEntity"
        |                }
        |              }
        |            }
        |          }
        |        },
        |        "operationId": "post_multiParamPost",
        |        "parameters": [
        |          {
        |            "name": "p1",
        |            "in": "path",
        |            "required": true,
        |            "schema": {
        |              "type": "integer",
        |              "format": "int32"
        |            }
        |          },
        |          {
        |            "name": "p2",
        |            "in": "path",
        |            "required": true,
        |            "schema": {
        |              "type": "string"
        |            }
        |          },
        |          {
        |            "name": "X-H1",
        |            "in": "header",
        |            "required": true,
        |            "schema": {
        |              "type": "integer",
        |              "format": "int32"
        |            }
        |          },
        |          {
        |            "name": "X-H2",
        |            "in": "header",
        |            "required": true,
        |            "schema": {
        |              "type": "string"
        |            }
        |          },
        |          {
        |            "name": "q1",
        |            "in": "query",
        |            "required": true,
        |            "schema": {
        |              "type": "integer",
        |              "format": "int32"
        |            }
        |          },
        |          {
        |            "name": "q=2",
        |            "in": "query",
        |            "required": true,
        |            "schema": {
        |              "type": "string"
        |            }
        |          }
        |        ],
        |        "requestBody": {
        |          "content": {
        |            "application/json": {
        |              "schema": {
        |                "type": "object",
        |                "properties": {
        |                  "b1": {
        |                    "type": "integer",
        |                    "format": "int32"
        |                  },
        |                  "b\"2": {
        |                    "type": "string",
        |                    "description": "weird body field"
        |                  }
        |                },
        |                "required": [
        |                  "b1",
        |                  "b\"2"
        |                ]
        |              }
        |            }
        |          },
        |          "required": true
        |        }
        |      }
        |    },
        |    "/a/b/{p1}/p1/{p2}": {
        |      "get": {
        |        "responses": {
        |          "200": {
        |            "content": {
        |              "application/json": {
        |                "schema": {
        |                  "$ref": "#/components/schemas/RestEntity"
        |                }
        |              }
        |            }
        |          }
        |        },
        |        "description": "A really complex GET operation",
        |        "operationId": "get_complexGet",
        |        "parameters": [
        |          {
        |            "name": "p1",
        |            "in": "path",
        |            "required": true,
        |            "schema": {
        |              "type": "integer",
        |              "format": "int32"
        |            }
        |          },
        |          {
        |            "name": "p2",
        |            "in": "path",
        |            "description": "Very serious path parameter",
        |            "required": true,
        |            "schema": {
        |              "type": "string"
        |            }
        |          },
        |          {
        |            "name": "X-H1",
        |            "in": "header",
        |            "required": true,
        |            "schema": {
        |              "type": "integer",
        |              "format": "int32"
        |            }
        |          },
        |          {
        |            "name": "X-H2",
        |            "in": "header",
        |            "required": true,
        |            "schema": {
        |              "type": "string"
        |            }
        |          },
        |          {
        |            "name": "q1",
        |            "in": "query",
        |            "required": true,
        |            "schema": {
        |              "type": "integer",
        |              "format": "int32"
        |            }
        |          },
        |          {
        |            "name": "q=2",
        |            "in": "query",
        |            "required": true,
        |            "schema": {
        |              "type": "string"
        |            }
        |          }
        |        ]
        |      }
        |    },
        |    "/": {
        |      "put": {
        |        "responses": {
        |          "200": {
        |            "content": {
        |              "application/json": {
        |                "schema": {
        |                  "type": "string"
        |                }
        |              }
        |            }
        |          }
        |        },
        |        "operationId": "put_singleBodyPut",
        |        "requestBody": {
        |          "content": {
        |            "application/json": {
        |              "schema": {
        |                "description": "REST entity description",
        |                "allOf": [
        |                  {
        |                    "$ref": "#/components/schemas/RestEntity"
        |                  }
        |                ]
        |              }
        |            }
        |          },
        |          "required": true
        |        }
        |      }
        |    }
        |  },
        |  "servers": [
        |    {
        |      "url": "http://localhost"
        |    }
        |  ],
        |  "components": {
        |    "schemas": {
        |      "RestOtherEntity": {
        |        "type": "object",
        |        "properties": {
        |          "_case": {
        |            "type": "string",
        |            "enum": [
        |              "RestOtherEntity"
        |            ]
        |          },
        |          "fuu": {
        |            "type": "boolean"
        |          },
        |          "kek": {
        |            "type": "array",
        |            "items": {
        |              "type": "string"
        |            }
        |          }
        |        },
        |        "required": [
        |          "_case",
        |          "fuu",
        |          "kek"
        |        ]
        |      },
        |      "BaseEntity": {
        |        "oneOf": [
        |          {
        |            "type": "object",
        |            "properties": {
        |              "RestEntity": {
        |                "$ref": "#/components/schemas/RestEntity"
        |              }
        |            },
        |            "required": [
        |              "RestEntity"
        |            ]
        |          },
        |          {
        |            "type": "object",
        |            "properties": {
        |              "RestOtherEntity": {
        |                "type": "object",
        |                "properties": {
        |                  "fuu": {
        |                    "type": "boolean"
        |                  },
        |                  "kek": {
        |                    "type": "array",
        |                    "items": {
        |                      "type": "string"
        |                    }
        |                  }
        |                },
        |                "required": [
        |                  "fuu",
        |                  "kek"
        |                ]
        |              }
        |            },
        |            "required": [
        |              "RestOtherEntity"
        |            ]
        |          },
        |          {
        |            "type": "object",
        |            "properties": {
        |              "SingletonEntity": {
        |                "type": "object"
        |              }
        |            },
        |            "required": [
        |              "SingletonEntity"
        |            ]
        |          }
        |        ]
        |      },
        |      "FlatBaseEntity": {
        |        "description": "Flat sealed entity with some serious cases",
        |        "oneOf": [
        |          {
        |            "$ref": "#/components/schemas/taggedRestEntity"
        |          },
        |          {
        |            "$ref": "#/components/schemas/RestOtherEntity"
        |          },
        |          {
        |            "$ref": "#/components/schemas/SingletonEntity"
        |          }
        |        ],
        |        "discriminator": {
        |          "propertyName": "_case",
        |          "mapping": {
        |            "RestEntity": "taggedRestEntity"
        |          }
        |        }
        |      },
        |      "RestEntity": {
        |        "type": "object",
        |        "description": "REST entity",
        |        "properties": {
        |          "id": {
        |            "type": "string",
        |            "description": "entity id"
        |          },
        |          "name": {
        |            "type": "string"
        |          },
        |          "subentity": {
        |            "description": "recursive optional subentity",
        |            "nullable": true,
        |            "allOf": [
        |              {
        |                "$ref": "#/components/schemas/RestEntity"
        |              }
        |            ]
        |          }
        |        },
        |        "required": [
        |          "id",
        |          "name"
        |        ]
        |      },
        |      "taggedRestEntity": {
        |        "allOf": [
        |          {
        |            "type": "object",
        |            "properties": {
        |              "_case": {
        |                "type": "string",
        |                "enum": [
        |                  "RestEntity"
        |                ]
        |              }
        |            },
        |            "required": [
        |              "_case"
        |            ]
        |          },
        |          {
        |            "$ref": "#/components/schemas/RestEntity"
        |          }
        |        ]
        |      },
        |      "SingletonEntity": {
        |        "type": "object",
        |        "properties": {
        |          "_case": {
        |            "type": "string",
        |            "enum": [
        |              "SingletonEntity"
        |            ]
        |          }
        |        },
        |        "required": [
        |          "_case"
        |        ]
        |      }
        |    }
        |  }
        |}""".stripMargin
    )
  }
}
