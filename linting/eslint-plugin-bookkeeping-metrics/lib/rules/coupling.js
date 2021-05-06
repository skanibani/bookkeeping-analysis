/**
 * @fileoverview Reports all imports and required modules per module.
 * @author Skander Bibani
 */
"use strict";

//------------------------------------------------------------------------------
// Rule Definition
//------------------------------------------------------------------------------

module.exports = {
    meta: {
        type: "problem",
        docs: {
            description: "Reports all imports and required modules per module.",
            category: "Metrics",
            recommended: false
        },
        fixable: null,  // or "code" or "whitespace"
        schema: [
            // fill in your schema
        ]
    },

    create: function(context) {


        return {


            ImportDeclaration: function(node) {
                const importModule = node.source.value;

                context.report({
                    node: node,
                    message: importModule
                });
            },

            CallExpression(node) {

                if (node.callee.name == "require") {

                    const requireModule = node.arguments[0].value;
                    
                    context.report({
                        node: node,
                        message: requireModule
                    });
                }
            }
        };
    }
};
