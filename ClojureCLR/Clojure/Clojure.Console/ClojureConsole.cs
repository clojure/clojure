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
using Microsoft.Scripting.Hosting.Shell;
using clojure.lang;
using System.IO;
using Microsoft.Linq.Expressions;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting;
using Microsoft.Scripting.Runtime;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Hosting.Providers;
using System.Diagnostics;
using System.Resources;
using System.Threading;


using clojure.runtime;
using clojure.compiler;


namespace clojure.console
{
    class ClojureConsole : ConsoleHost, Compiler.EEHooks
    {
        #region Data

        private bool _isInitialized = false;

        #endregion

        #region Convenience accessors

        private ClojureContext GetLanguageContext()
        {
            return (ClojureContext)HostingHelpers.GetLanguageContext(Engine);
        }


        private static SourceUnit GetSourceUnit(ScriptSource scriptSource)
        {
            return HostingHelpers.GetSourceUnit(scriptSource);
        }

        #endregion

        #region Basic overrides

        protected override Type Provider
        {
            get
            {
                return typeof(ClojureContext);
            }
        }

        protected override CommandLine CreateCommandLine()
        {
            return new ClojureCommandLine();
        }

        #endregion

        #region Main routine

        [STAThread]
        static int Main(string[] args)
        {
            ClojureConsole cc = new ClojureConsole();

            int ret = cc.Run(args);

            Console.ReadLine();
            return ret;
        }

        #endregion

        #region Execution override

        protected override void ExecuteInternal()
        {
            Debug.Assert(Engine != null);

            Var.pushThreadBindings(
                RT.map(RT.CURRENT_NS, RT.CURRENT_NS.deref()));
            try
            {
                Snippets.SetSaveAssemblies(true, ".");
                MaybeInitialize(); 
                Snippets.SaveAndVerifyAssemblies();
                base.ExecuteInternal();
            }
            catch (Exception e)
            {
                UnhandledException(Engine, e);
            }

            finally
            {
                Snippets.SaveAndVerifyAssemblies();
                Var.popThreadBindings();
            }
        }


        #endregion

        #region Initialization

        private void MaybeInitialize()
        {
            if (_isInitialized)
                return;

            _isInitialized = true;

            Compiler.SetHooks(this);

            Stopwatch sw = new Stopwatch();
            sw.Start();

            LoadFromStream(new StringReader(clojure.properties.Resources.core));
            LoadFromStream(new StringReader(clojure.properties.Resources.core_print));
            LoadFromStream(new StringReader(clojure.properties.Resources.test));

            sw.Stop();
            Console.WriteLine("Loading took {0} milliseconds.", sw.ElapsedMilliseconds);


        }

        #endregion

        #region EEHooks Members

        public object Eval(object form)
        {
            ScriptSource scriptSource = Engine.CreateScriptSourceFromString("<internal>");

            Expression expr = Generator.Eval(GetLanguageContext(), form);
            LambdaExpression ast = Expression.Lambda(expr);
            ast = new GlobalLookupRewriter().RewriteLambda(ast);
            ScriptCode code = new ScriptCode(ast, GetSourceUnit(scriptSource));
            return code.Run();
        }

        public object Macroexpand1(object form)
        {
            return Generator.Macroexpand1(GetLanguageContext(), form);
        }

        public object LoadFromStream(TextReader rdr)
        {
            ScriptSource scriptSource = Engine.CreateScriptSourceFromString("<already opened TextReader>");
            //PushbackReader pbr = new PushbackReader(rdr);

            return LoadFromPushbackReader(scriptSource, rdr, false);
        }

        public object LoadFile(string filename)
        {
            ScriptSource scriptSource = Engine.CreateScriptSourceFromFile(filename);

            return LoadFromPushbackReader(scriptSource, scriptSource.GetReader(), false);
        }

        private static object LoadFromPushbackReader(ScriptSource scriptSource, TextReader pbr, bool addPrint)
        {
            object ret = null;
            object eofVal = new object();
            object form;
            while ((form = LispReader.read(pbr, false, eofVal, false)) != eofVal)
            {
                LambdaExpression ast = Generator.Generate(form, addPrint);
                ast = new GlobalLookupRewriter().RewriteLambda(ast);

                ScriptCode code = new ScriptCode(ast, GetSourceUnit(scriptSource));
                ret = code.Run();
            }

            return ret;
        }

        public Delegate GenerateTypedDelegate(Type delegateType, Symbol optName, IPersistentVector argList, ISeq body)
        {
            ScriptSource scriptSource = Engine.CreateScriptSourceFromString("<internal>");

            LambdaExpression ast = Generator.GenerateTypedDelegateExpression(GetLanguageContext(), delegateType, optName, argList, body);
            return ast.Compile(); 

            //ast = new GlobalLookupRewriter().RewriteLambda(ast);  -- doesn't work unless no args
            //ScriptCode code = new ScriptCode(ast, GetSourceUnit(scriptSource));
            //return code;
        }

        #endregion

        protected override ScriptRuntimeSetup CreateRuntimeSetup()
        {
            ScriptRuntimeSetup setup =  base.CreateRuntimeSetup();
            setup.DebugMode = true;
            return setup;
        }
    }
}
