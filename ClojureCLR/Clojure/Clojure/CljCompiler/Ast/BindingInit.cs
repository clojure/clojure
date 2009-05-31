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

namespace clojure.lang.CljCompiler.Ast
{
    struct BindingInit
    {
        #region Data

        private readonly LocalBinding _binding;
        public LocalBinding Binding
        {
            get { return _binding; }
        }

        private readonly Expr _init;
        public Expr Init
        {
            get { return _init; }
        }

        public BindingInit(LocalBinding binding, Expr init)
        {
            _binding = binding;
            _init = init;
        }

        #endregion
    }
}
