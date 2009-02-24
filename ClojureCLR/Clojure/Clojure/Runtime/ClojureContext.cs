/**
 *   Copyright (c) David Miller. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.Scripting.Runtime;
using Microsoft.Linq.Expressions;
using Microsoft.Scripting;
using Microsoft.Scripting.Generation;
using clojure.lang;
using clojure.compiler;

namespace clojure.runtime
{
    public class ClojureContext : LanguageContext
    {
        public ClojureContext(ScriptDomainManager manager, IDictionary<string, object> options)
            : base(manager)
        {
            //Binder = new ClojureBinder(manager);
            manager.LoadAssembly(typeof(string).Assembly);
            manager.LoadAssembly(typeof(ISeq).Assembly);
        }

        protected override Microsoft.Scripting.ScriptCode CompileSourceCode(Microsoft.Scripting.SourceUnit sourceUnit, Microsoft.Scripting.CompilerOptions options, Microsoft.Scripting.ErrorSink errorSink)
        {
            ClojureParser cp = new ClojureParser(sourceUnit);
            LambdaExpression ast;

            switch (sourceUnit.Kind)
            {
                case SourceCodeKind.InteractiveCode:
                    {
                        ScriptCodeParseResult result;
                        object code = cp.ParseInteractiveStatement(out result);
                        sourceUnit.CodeProperties = result;
                        if (result != ScriptCodeParseResult.Complete)
                            return null;
                        ast = Generator.Generate(code, true);
                    }
                    break;

                default:
                    sourceUnit.CodeProperties = ScriptCodeParseResult.Complete;
                    ast = Generator.Generate(cp.ParseFile(), sourceUnit);
                    break;
            }

            ast = new GlobalLookupRewriter().RewriteLambda(ast);
            return new ScriptCode(ast, sourceUnit);
        }
    }
}
