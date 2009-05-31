using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using NUnit.Framework;
using clojure.lang;


    [SetUpFixture]    
    public class FixtureSetupClass
    {
        [SetUp]
        public void Setup()
        {
            RT_Bootstrap_Flag._doRTBootstrap = false;
        }
    }

