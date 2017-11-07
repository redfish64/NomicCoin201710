<map version="1.0.1">
<!-- To view this file, download free mind mapping software FreeMind from http://freemind.sourceforge.net -->
<node CREATED="1510032772531" ID="ID_728410656" MODIFIED="1510032842596" TEXT="Autonomic">
<node CREATED="1482662686966" FOLDED="true" ID="ID_259911855" MODIFIED="1510032830172" POSITION="right" TEXT="agda">
<node CREATED="1482662689102" ID="ID_572924383" MODIFIED="1482666839954" TEXT="internals">
<node CREATED="1482666839854" ID="ID_555055730" MODIFIED="1506060790644" TEXT="impl">
<node CREATED="1482662691325" ID="ID_543510742" MODIFIED="1482663962041" TEXT="syntaxes">
<node CREATED="1482663969917" ID="ID_1625854104" MODIFIED="1482663971745" TEXT="Concrete">
<node CREATED="1482664061637" ID="ID_377224993" MODIFIED="1482664067351">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      {-| The concrete syntax is a raw representation of the program text
    </p>
    <p>
      &#160;&#160;&#160;&#160;without any desugaring at all.&#160;&#160;This is what the parser produces.
    </p>
    <p>
      &#160;&#160;&#160;&#160;The idea is that if we figure out how to keep the concrete syntax
    </p>
    <p>
      &#160;&#160;&#160;&#160;around, it can be printed exactly as the user wrote it.
    </p>
    <p>
      -}
    </p>
  </body>
</html></richcontent>
</node>
</node>
<node CREATED="1482663972077" ID="ID_854482047" MODIFIED="1482664554620" TEXT="Abstract">
<node CREATED="1482664554593" ID="ID_1715770572" MODIFIED="1482664559985" TEXT="spec">
<node CREATED="1482664034757" ID="ID_1440578939" MODIFIED="1482664040516">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      {-| The abstract syntax. This is what you get after desugaring and scope
    </p>
    <p>
      &#160;&#160;&#160;&#160;analysis of the concrete syntax. The type checker works on abstract syntax,
    </p>
    <p>
      &#160;&#160;&#160;&#160;producing internal syntax (&quot;Agda.Syntax.Internal&quot;).
    </p>
    <p>
      -}
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1482664416069" ID="ID_1862404543" MODIFIED="1482664418249" TEXT="Telescope">
<node CREATED="1482664454013" ID="ID_1301660286" MODIFIED="1482664464233" TEXT="Some sort of type binding expression">
<node CREATED="1482664465485" ID="ID_594082145" MODIFIED="1482664480836">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      &#160;&#160;= TBind Range [WithHiding Name] Expr
    </p>
    <p>
      &#160;&#160;&#160;&#160;-- ^ As in telescope @(x y z : A)@ or type @(x y z : A) -&gt; B@.
    </p>
  </body>
</html></richcontent>
</node>
</node>
<node CREATED="1482664493046" ID="ID_1915549362" MODIFIED="1482664508106" TEXT="Defined as [TypeBinding]"/>
</node>
</node>
</node>
<node CREATED="1482663963565" ID="ID_714129684" MODIFIED="1482664073543" TEXT="Internal">
<node CREATED="1482664844237" ID="ID_1691032342" MODIFIED="1482664849562" TEXT="??? after type checker"/>
</node>
<node CREATED="1482663974173" ID="ID_1678687605" MODIFIED="1482663976953" TEXT="Treeless">
<node CREATED="1482664009045" ID="ID_1764711421" MODIFIED="1482664014172">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      -- | The treeless syntax is intended to be used as input for the compiler backends.
    </p>
    <p>
      -- It is more low-level than Internal syntax and is not used for type checking.
    </p>
    <p>
      --
    </p>
    <p>
      -- Some of the features of treeless syntax are:
    </p>
    <p>
      -- - case expressions instead of case trees
    </p>
    <p>
      -- - no instantiated datatypes / constructors
    </p>
  </body>
</html></richcontent>
</node>
</node>
</node>
<node CREATED="1482666842381" ID="ID_1477258993" MODIFIED="1482666849170" TEXT="translation between syntaxes">
<node CREATED="1482666850078" ID="ID_1200570358" MODIFIED="1482666858074" TEXT="ConcreteToAbstract.hs">
<node CREATED="1482666858958" ID="ID_1743588553" MODIFIED="1482666871863">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      {-| Translation from &quot;Agda.Syntax.Concrete&quot; to &quot;Agda.Syntax.Abstract&quot;. Involves scope analysis,
    </p>
    <p>
      &#160;&#160;&#160;&#160;figuring out infix operator precedences and tidying up definitions.
    </p>
    <p>
      -}
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1482667139110" ID="ID_230828936" MODIFIED="1482667399706" TEXT="Only referenced under Agda/Interaction/* classes">
<node CREATED="1482667400518" ID="ID_78864587" MODIFIED="1482667411354" TEXT="(these classes handle compilation of files, too"/>
</node>
</node>
</node>
<node CREATED="1482667431910" ID="ID_264038523" MODIFIED="1489479935140" TEXT="compilation">
<node CREATED="1482667439230" ID="ID_427053301" MODIFIED="1482667472258" TEXT="Agda.Interaction.Imports.typeCheckMain">
<node CREATED="1482717298526" ID="ID_322081371" MODIFIED="1490438657826" TEXT="calls getInterface&apos;">
<node CREATED="1490438657694" ID="ID_756412276" MODIFIED="1490438662717" TEXT="calls typeCheck">
<node CREATED="1482717310110" ID="ID_1410145942" MODIFIED="1483006910957" TEXT="calls createInterface (within typeCheckTheFile)"/>
</node>
</node>
</node>
<node CREATED="1483008925045" ID="ID_563020826" MODIFIED="1483009177212" TEXT="createInterface">
<node CREATED="1483008382747" ID="ID_1562763610" MODIFIED="1483008934006" TEXT="main function for compilation">
<node CREATED="1483008394266" ID="ID_927227069" MODIFIED="1483008435093" TEXT="It doesn&apos;t generate an Abstract tree, though, as its final product"/>
</node>
<node CREATED="1483009177187" ID="ID_844788651" MODIFIED="1483009178519" TEXT="details">
<node CREATED="1483008934586" ID="ID_960340006" MODIFIED="1483008950591" TEXT="calls parseFile&apos; with a moduleParser">
<node CREATED="1483008978474" ID="ID_1033971219" MODIFIED="1483008985605" TEXT="This seems to generate a concrete tree"/>
<node CREATED="1483009006786" ID="ID_723847740" MODIFIED="1483009016623" TEXT="as well as some sort of pragma representation"/>
<node CREATED="1483009019186" ID="ID_1452281716" MODIFIED="1483009029119" TEXT="outputs are &quot;pragmas&quot; and &quot;top&quot;"/>
</node>
<node CREATED="1483009084251" ID="ID_915051940" MODIFIED="1483009098646" TEXT="pragmas get turned into an abstract tree and redefined as such"/>
<node CREATED="1483009166403" ID="ID_1334674117" MODIFIED="1483009198966" TEXT="top is converted to abstract data as &quot;topLevel&quot;">
<node CREATED="1483009200403" ID="ID_1488584552" MODIFIED="1483009204678" TEXT="topLevel">
<node CREATED="1483009205610" ID="ID_138879424" MODIFIED="1483009229742" TEXT="contains">
<node CREATED="1483009231258" ID="ID_1748172839" MODIFIED="1483009233990" TEXT="topLevelDecls"/>
<node CREATED="1483009234330" ID="ID_1745142385" MODIFIED="1483009240822" TEXT="topLevelScope"/>
</node>
</node>
</node>
<node CREATED="1483009363027" ID="ID_1041941708" MODIFIED="1483009392039" TEXT="topLevel field &quot;ds&quot; is type checked (unless it is in some sort of cache)">
<node CREATED="1483009393011" ID="ID_202726853" MODIFIED="1483009404910" TEXT="done by checkDeclCached">
<node CREATED="1483009411978" ID="ID_106643337" MODIFIED="1483009418591" TEXT="This calls checkDecl">
<node CREATED="1483009445586" ID="ID_1292049774" MODIFIED="1483009455654" TEXT="this does the type checking"/>
</node>
</node>
</node>
<node CREATED="1483010019651" ID="ID_1027838818" MODIFIED="1483010063247" TEXT="It then creates the interface using &quot;buildInterface&quot;, but the topLevelDecls are not used">
<node CREATED="1483010097786" ID="ID_281830760" MODIFIED="1483010117118" TEXT="Maybe this is because the compilers work against the concrete tree, so they don&apos;t need the abstract data">
<node CREATED="1483010122042" ID="ID_547793783" MODIFIED="1483010144400">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      But then how does evaling a loaded function work?
    </p>
  </body>
</html></richcontent>
</node>
</node>
</node>
</node>
</node>
</node>
<node CREATED="1483003106344" ID="ID_255814478" MODIFIED="1489479955047" TEXT="getting abstract syntax">
<node CREATED="1483003111248" ID="ID_1485670618" MODIFIED="1483003127435" TEXT="Agda.Interactive.CommandLine.parseExpr">
<node CREATED="1483003131672" ID="ID_1880386880" MODIFIED="1483003144038" TEXT="takes a string a returns an TCM monad with an abstract expression inside of it"/>
<node CREATED="1483004125537" ID="ID_1193732425" MODIFIED="1483004132476" TEXT="needs code inside of &quot;let&quot;"/>
<node CREATED="1483004133304" ID="ID_911094450" MODIFIED="1483004146166" TEXT="&quot;let foo : Set\n    foo = 42\nin foo&quot; works"/>
<node CREATED="1503447959109" ID="ID_1767041175" MODIFIED="1503447974976" TEXT="creates an &quot;exprParser&quot;">
<node CREATED="1503447986107" ID="ID_23962253" MODIFIED="1503447994424">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      exprParser :: Parser Expr
    </p>
    <p>
      exprParser = Parser { parser = P.exprParser
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;, parseFlags = withoutComments
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;, parseLiterate = parseLiterateWithoutComments
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;}
    </p>
  </body>
</html></richcontent>
<node CREATED="1503448023167" ID="ID_1489687959" MODIFIED="1503448037783" TEXT="&quot;parser = P.exprParser&quot; is the happy parser"/>
</node>
</node>
<node CREATED="1503448086539" ID="ID_710800717" MODIFIED="1503448192119" TEXT="Runs Agda.Syntax.Parser.parse on &quot;exprParser&quot; and string to parse"/>
</node>
<node CREATED="1483013939445" ID="ID_608577804" MODIFIED="1483013965017" TEXT="Note that command line doesn&apos;t work at all. ">
<node CREATED="1483013966213" ID="ID_1439269911" MODIFIED="1483013967128" TEXT=":load doesn&apos;t complain even when file is missng"/>
<node CREATED="1483013967565" ID="ID_1426609855" MODIFIED="1483013988488" TEXT="parseExpr takes a string, but doesn&apos;t take any context at all. Loading a file and trying to run things don&apos;t work at all"/>
</node>
<node CREATED="1483014096941" ID="ID_383243470" MODIFIED="1483014103000" TEXT="mimicGHCi">
<node CREATED="1483014104365" ID="ID_1836242807" MODIFIED="1483014115336" TEXT="this is the maintained emacs interface"/>
<node CREATED="1483096569433" ID="ID_634704640" MODIFIED="1483096571796" TEXT="calls">
<node CREATED="1483096572968" ID="ID_687164365" MODIFIED="1483096578332" TEXT="runInteraction">
<node CREATED="1483096699632" ID="ID_706088778" MODIFIED="1483096704164" TEXT="commands are Interaction&apos;">
<node CREATED="1483096705936" ID="ID_208604982" MODIFIED="1483096921205" TEXT="uses reads to read these (wrapped in some other object, done in mimicGHCi)"/>
<node CREATED="1483143707250" ID="ID_745878638" MODIFIED="1483143720734" TEXT="emacs commands are done as a CommandM monad">
<node CREATED="1483143721850" ID="ID_123371460" MODIFIED="1483143785320" TEXT="cmd_load&apos; loads a file">
<node CREATED="1483143785306" ID="ID_1751218921" MODIFIED="1483143786165" TEXT="calls">
<node CREATED="1483143734538" ID="ID_1164604590" MODIFIED="1483143784214" TEXT="Imp.typeCheckMain">
<node CREATED="1483143909738" ID="ID_190144527" MODIFIED="1483143914638" TEXT="calls getInterface&apos;"/>
</node>
</node>
<node CREATED="1483143791258" ID="ID_1795834008" MODIFIED="1483143807637" TEXT="then lifts the result into a CommandM in some fashion"/>
</node>
</node>
</node>
<node CREATED="1483242074011" ID="ID_219181276" MODIFIED="1483242077798" TEXT="calls">
<node CREATED="1483242078746" ID="ID_490629622" MODIFIED="1483242086358" TEXT="interpret">
<node CREATED="1483242092306" ID="ID_766012554" MODIFIED="1483242104782" TEXT="calls (for Cmd_load)">
<node CREATED="1483242095115" ID="ID_1758339101" MODIFIED="1483242098262" TEXT="cmd_load&apos;"/>
</node>
</node>
</node>
</node>
</node>
<node CREATED="1483185992760" ID="ID_713267603" MODIFIED="1483185994747" TEXT="example">
<node CREATED="1483185995862" ID="ID_792482846" MODIFIED="1483186000738" TEXT="&quot;Agda2&gt;&quot;">
<node CREATED="1483186001678" ID="ID_1296012727" MODIFIED="1483186022221">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      IOTCM &quot;test.agda&quot; None Indirect ( Cmd_load &quot;test.agda&quot; [] )
    </p>
  </body>
</html></richcontent>
</node>
</node>
</node>
</node>
</node>
<node CREATED="1484128878462" ID="ID_1086902838" MODIFIED="1489479943485" TEXT="Interface">
<node CREATED="1484128881574" ID="ID_837504645" MODIFIED="1484128899883" TEXT="This is the data after type checking, and is loaded when importing already compiled modules"/>
<node CREATED="1484128901190" ID="ID_850790587" MODIFIED="1484128910225" TEXT="We&apos;ll probably use this as data input for the tree"/>
<node CREATED="1484128911358" ID="ID_861313365" MODIFIED="1484128946712" TEXT="fields">
<node CREATED="1484128946670" ID="ID_1282114701" MODIFIED="1484128947697" TEXT="all">
<node CREATED="1484128917838" ID="ID_1074267745" MODIFIED="1484128932055">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      iSourceHash&#160;&#160;&#160;&#160;&#160;&#160;:: Hash
    </p>
    <p>
      &#160;&#160;&#160;&#160;-- ^ Hash of the source code.
    </p>
    <p>
      &#160;&#160;, iImportedModules :: [(ModuleName, Hash)]
    </p>
    <p>
      &#160;&#160;&#160;&#160;-- ^ Imported modules and their hashes.
    </p>
    <p>
      &#160;&#160;, iModuleName&#160;&#160;&#160;&#160;&#160;&#160;:: ModuleName
    </p>
    <p>
      &#160;&#160;&#160;&#160;-- ^ Module name of this interface.
    </p>
    <p>
      &#160;&#160;, iScope&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;:: Map ModuleName Scope
    </p>
    <p>
      &#160;&#160;&#160;&#160;-- ^ Scope defined by this module.
    </p>
    <p>
      &#160;&#160;, iInsideScope&#160;&#160;&#160;&#160;&#160;:: ScopeInfo
    </p>
    <p>
      &#160;&#160;&#160;&#160;-- ^ Scope after we loaded this interface.
    </p>
    <p>
      &#160;&#160;&#160;&#160;--&#160;&#160;&#160;Used in 'Agda.Interaction.BasicOps.AtTopLevel'
    </p>
    <p>
      &#160;&#160;&#160;&#160;--&#160;&#160;&#160;and&#160;&#160;&#160;&#160;&#160;'Agda.Interaction.CommandLine.interactionLoop'.
    </p>
    <p>
      &#160;&#160;&#160;&#160;--
    </p>
    <p>
      &#160;&#160;&#160;&#160;--&#160;&#160;&#160;Andreas, AIM XX: For performance reason, this field is
    </p>
    <p>
      &#160;&#160;&#160;&#160;--&#160;&#160;&#160;not serialized, so if you deserialize an interface, @iInsideScope@
    </p>
    <p>
      &#160;&#160;&#160;&#160;--&#160;&#160;&#160;will be empty.&#160;&#160;You need to type-check the file to get @iInsideScope@.
    </p>
    <p>
      &#160;&#160;, iSignature&#160;&#160;&#160;&#160;&#160;&#160;&#160;:: Signature
    </p>
    <p>
      &#160;&#160;, iDisplayForms&#160;&#160;&#160;&#160;:: DisplayForms
    </p>
    <p>
      &#160;&#160;&#160;&#160;-- ^ Display forms added for imported identifiers.
    </p>
    <p>
      &#160;&#160;, iBuiltin&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;:: BuiltinThings (String, QName)
    </p>
    <p>
      &#160;&#160;, iHaskellImports&#160;&#160;:: Set String
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- ^ Haskell imports listed in
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- (transitively) imported modules are
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- not included here.
    </p>
    <p>
      &#160;&#160;, iHaskellImportsUHC :: Set String
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- ^ Haskell imports listed in
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- (transitively) imported modules are
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- not included here.
    </p>
    <p>
      &#160;&#160;, iHaskellCode&#160;&#160;&#160;&#160;&#160;:: [String] -- ^ Inline Haskell code
    </p>
    <p>
      &#160;&#160;, iHighlighting&#160;&#160;&#160;&#160;:: HighlightingInfo
    </p>
    <p>
      &#160;&#160;, iPragmaOptions&#160;&#160;&#160;:: [OptionsPragma]
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- ^ Pragma options set in the file.
    </p>
    <p>
      &#160;&#160;, iPatternSyns&#160;&#160;&#160;&#160;&#160;:: A.PatternSynDefns
    </p>
  </body>
</html></richcontent>
</node>
</node>
<node CREATED="1484128948414" ID="ID_554723132" MODIFIED="1484128998802" TEXT="iSignature">
<node CREATED="1484128999813" ID="ID_1404617196" MODIFIED="1484129006226" TEXT="This is where all the code goes, it seems"/>
<node CREATED="1484129155942" ID="ID_373762677" MODIFIED="1484129180026">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      data Signature = Sig
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;{ _sigSections&#160;&#160;&#160;&#160;:: Sections
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;, _sigDefinitions :: Definitions
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;, _sigRewriteRules:: RewriteRuleMap&#160;&#160;-- ^ The rewrite rules defined in this file.
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;}
    </p>
    <p>
      &#160;&#160;deriving (Typeable, Show)
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1484129185574" ID="ID_385677357" MODIFIED="1484129187193" TEXT="Sections"/>
<node CREATED="1484129187478" ID="ID_761476327" MODIFIED="1484129189681" TEXT="Definitions"/>
</node>
<node CREATED="1484129049623" ID="ID_1865438648" MODIFIED="1484129052321" TEXT="iPatternSyns">
<node CREATED="1484129148126" ID="ID_1996947996" MODIFIED="1484129155219" TEXT="not sure what this is, but looks important"/>
<node CREATED="1484129053518" ID="ID_53069526" MODIFIED="1484129104458" TEXT="type PatternSynDefn = ([Arg Name], Pattern&apos; Void) &#xa;type PatternSynDefns = Map QName PatternSynDefn "/>
<node CREATED="1484129113078" ID="ID_754256561" MODIFIED="1484129115578" TEXT="Pattern&apos;">
<node CREATED="1484129117278" ID="ID_1676233939" MODIFIED="1484129117945" TEXT="all">
<node CREATED="1484129119438" ID="ID_1358482395" MODIFIED="1484129137384">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      data Pattern' e
    </p>
    <p>
      &#160;&#160;= VarP Name
    </p>
    <p>
      &#160;&#160;| ConP ConPatInfo AmbiguousQName [NamedArg (Pattern' e)]
    </p>
    <p>
      &#160;&#160;| DefP PatInfo QName&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;[NamedArg (Pattern' e)]
    </p>
    <p>
      &#160;&#160;&#160;&#160;-- ^ Defined pattern: function definition @f ps@ or destructor pattern @d p ps@.
    </p>
    <p>
      &#160;&#160;| WildP PatInfo
    </p>
    <p>
      &#160;&#160;&#160;&#160;-- ^ Underscore pattern entered by user.
    </p>
    <p>
      &#160;&#160;&#160;&#160;--&#160;&#160;&#160;Or generated at type checking for implicit arguments.
    </p>
    <p>
      &#160;&#160;| AsP PatInfo Name (Pattern' e)
    </p>
    <p>
      &#160;&#160;| DotP PatInfo e
    </p>
    <p>
      &#160;&#160;| AbsurdP PatInfo
    </p>
    <p>
      &#160;&#160;| LitP Literal
    </p>
    <p>
      &#160;&#160;| PatternSynP PatInfo QName [NamedArg (Pattern' e)]
    </p>
    <p>
      &#160;&#160;| RecP PatInfo [FieldAssignment' (Pattern' e)]
    </p>
    <p>
      &#160;&#160;deriving (Typeable, Show, Functor, Foldable, Traversable, Eq)
    </p>
  </body>
</html></richcontent>
</node>
</node>
</node>
</node>
</node>
</node>
</node>
<node CREATED="1483146608515" FOLDED="true" ID="ID_1238903845" MODIFIED="1503360048893" TEXT="usage">
<node CREATED="1483146613275" ID="ID_1628785136" MODIFIED="1483146643319" TEXT="getInterface loads a module in a way where it effects type checking for the next getInterface call">
<node CREATED="1483337471304" ID="ID_1541280997" MODIFIED="1483337480524" TEXT="One aspect of this is builtins">
<node CREATED="1483337481608" ID="ID_122084532" MODIFIED="1483337490340" TEXT="Builtins are updated in the TCM somehow"/>
<node CREATED="1483337492945" ID="ID_932705779" MODIFIED="1483337515140" TEXT="Whenever getInterface is called, it always imports the primitivie libraries with their own builtins first"/>
</node>
</node>
</node>
<node CREATED="1482664816941" FOLDED="true" ID="ID_75854235" MODIFIED="1503360049365" TEXT="plan">
<node CREATED="1482664819909" ID="ID_1243520786" MODIFIED="1482664828073" TEXT="Use Abstract syntax?">
<node CREATED="1482664560470" ID="ID_1996495556" MODIFIED="1482664577234" TEXT="may be want we need. Has app, pi, fun, etc."/>
<node CREATED="1482664615886" ID="ID_101580383" MODIFIED="1482664868583" TEXT="The type checker runs against this (I think)">
<node CREATED="1482664871533" ID="ID_259134019" MODIFIED="1482664892993" TEXT="We definitely need to perform our operations before the type checker runs"/>
<node CREATED="1482665188757" ID="ID_560668808" MODIFIED="1482665224425" TEXT="We need to be able to type check a little bit at a time for efficiency">
<node CREATED="1482665225317" ID="ID_718957075" MODIFIED="1482665251065" TEXT="The type checker must already be able to do this, at least on a module level"/>
</node>
</node>
<node CREATED="1482664582613" ID="ID_1688970565" MODIFIED="1482664611329" TEXT="If we can sanitize this, it may be good enough."/>
<node CREATED="1482665289677" ID="ID_939973632" MODIFIED="1482665295889" TEXT="What do we do after typecheck?">
<node CREATED="1482665296821" ID="ID_10906409" MODIFIED="1482665305634" TEXT="Interpret? Compile?"/>
<node CREATED="1482666255662" ID="ID_1467489980" MODIFIED="1482666689142">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      Agda has an (unsupported) interpreter, so it should also have the ability to type check in nibbles (small pieces of code incrementally)
    </p>
  </body>
</html></richcontent>
<node CREATED="1483169866540" ID="ID_1303112392" MODIFIED="1483169882495" TEXT="this interpreter doesn&apos;t work at all, but there is an emacs interface that can do much the same thing"/>
<node CREATED="1483169882787" ID="ID_304549665" MODIFIED="1483169898991" TEXT="except that it will recompile all imported files if they changed"/>
</node>
</node>
<node CREATED="1482664631661" ID="ID_1543213738" MODIFIED="1482664655689" TEXT="We will need the following operations">
<node CREATED="1482665284893" ID="ID_1853997898" MODIFIED="1482665286217" TEXT="???"/>
</node>
</node>
</node>
<node CREATED="1490698250419" FOLDED="true" ID="ID_1006589318" MODIFIED="1503360049989" TEXT="abstract syntax">
<node CREATED="1490698260771" ID="ID_1427781289" MODIFIED="1490698273142" TEXT="module test where &lt;nothing&gt;">
<node CREATED="1490698265578" ID="ID_945368944" MODIFIED="1490698293280">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      toplevelDecl is [Section (ModuleInfo {minfoRange = /home/tim/projects/autonomic3/app/test.agda:1,1-18, minfoAsTo = , minfoAsName = Nothing, minfoOpenShort = Nothing, minfoDirective = Nothing}) test [] []]
    </p>
    <p>
      toplevelScope is ScopeInfo
    </p>
    <p>
      &#160;&#160;current = test
    </p>
    <p>
      &#160;&#160;context = TopCtx
    </p>
    <p>
      &#160;&#160;modules
    </p>
    <p>
      &#160;&#160;&#160;&#160;scope
    </p>
    <p>
      &#160;&#160;&#160;&#160;scope test
    </p>
    <p>
      
    </p>
    <p>
      *** Exception: ExitSuccess
    </p>
  </body>
</html></richcontent>
</node>
</node>
</node>
</node>
<node CREATED="1503360053718" ID="ID_1643925668" MODIFIED="1503360055505" TEXT="draft2">
<node CREATED="1503360056661" ID="ID_1931249810" MODIFIED="1503360060616" TEXT="SPECIALIZE">
<node CREATED="1503360061245" ID="ID_1201512412" MODIFIED="1503360073409" TEXT="increases performance for certain types of calls"/>
</node>
<node CREATED="1503360078564" ID="ID_84537738" MODIFIED="1503360081145" TEXT="undefined.h">
<node CREATED="1503360082413" ID="ID_317661334" MODIFIED="1503360097137" TEXT="used to report line number when internal errors occur"/>
</node>
<node CREATED="1503360309164" ID="ID_906939832" MODIFIED="1503360318056" TEXT="-{# SOURCE #}-">
<node CREATED="1503360319108" ID="ID_1237334019" MODIFIED="1503360340944" TEXT="used to facilitate mutually recursive import statements"/>
</node>
</node>
</node>
<node CREATED="1487896640756" FOLDED="true" ID="ID_1257421444" MODIFIED="1503971118008" POSITION="right" TEXT="autonomic mining + solving">
<node CREATED="1487896663728" ID="ID_1755350453" MODIFIED="1487896697542" TEXT="(&#x2203; (&#x3bb; x &#x2192; sha256 sha256 (x ++ block_header) &#x2264; max_hash) &#xd7; *something else* ">
<node CREATED="1487896699529" ID="ID_860004145" MODIFIED="1487896717501" TEXT="*something else* is a stocastically defined problem">
<node CREATED="1487896737266" ID="ID_1079860689" MODIFIED="1487896823405" TEXT="using a block hash as a seed, randomly associated functions with same output type and a random relation"/>
<node CREATED="1487896764466" ID="ID_598425573" MODIFIED="1487896770701" TEXT="proof is then included"/>
</node>
<node CREATED="1487896771586" ID="ID_249507025" MODIFIED="1487896801365" TEXT="somehow, hard to solve problems get saved and incentive to solve them is created"/>
</node>
</node>
<node CREATED="1480463570259" FOLDED="true" ID="ID_646730677" MODIFIED="1510032826668" POSITION="right" TEXT="thoughts">
<node CREATED="1475895407904" ID="ID_643132779" MODIFIED="1475895414123" TEXT="agoras sub-context">
<node CREATED="1475895415457" ID="ID_62764785" MODIFIED="1475895475540" TEXT="provides subsidy to keep security up in the beginning?"/>
</node>
<node CREATED="1475895433927" ID="ID_956395962" MODIFIED="1475895435027" TEXT="miners">
<node CREATED="1475895436192" ID="ID_1624929700" MODIFIED="1475895446459" TEXT="should include every context out there that pays something"/>
</node>
<node CREATED="1475895482711" ID="ID_1992141380" MODIFIED="1475895485067" TEXT="sub-contexts">
<node CREATED="1475895486287" ID="ID_1369959473" MODIFIED="1475895503955" TEXT="how to keep chain around? Who does this?"/>
<node CREATED="1475895504840" ID="ID_137851122" MODIFIED="1475895528644" TEXT="How to notify miners they want to add a transaction?"/>
<node CREATED="1475895451874" ID="ID_100094042" MODIFIED="1475895455819" TEXT="how to contexts pay?">
<node CREATED="1475895456863" ID="ID_609897662" MODIFIED="1475895459715" TEXT="own coin"/>
<node CREATED="1475895459927" ID="ID_1284503909" MODIFIED="1475895461451" TEXT="agoras">
<node CREATED="1475895795127" ID="ID_420569694" MODIFIED="1475895844955" TEXT="somehow say money transferred from account x to miner if miner includes it in the block">
<node CREATED="1475895848911" ID="ID_1800850612" MODIFIED="1475895860939" TEXT="But how does it publish to the agoras chain?"/>
</node>
</node>
</node>
</node>
<node CREATED="1475896018471" ID="ID_1004687913" MODIFIED="1475896026827" TEXT="where do contracts involving agoras live?">
<node CREATED="1475896028409" ID="ID_1071992605" MODIFIED="1475896031523" TEXT="On agoras chain?"/>
<node CREATED="1475896040927" ID="ID_492510783" MODIFIED="1475896103243" TEXT="Can someone sign a transaction and place in another sub-context which is executable in agoras?">
<node CREATED="1475896104711" ID="ID_229599602" MODIFIED="1475896108364" TEXT="Seems so, doesn&apos;t it?"/>
</node>
</node>
<node CREATED="1475904726570" ID="ID_1975517830" MODIFIED="1475904764952" TEXT="can one sub-chain authorize agoras to pay an amount?">
<node CREATED="1475904678180" ID="ID_238087288" MODIFIED="1475904685832" TEXT="can chains rely on other chains?">
<node CREATED="1475904687668" ID="ID_1461251486" MODIFIED="1475904704568" TEXT="What if that other chain dies (no one bothers holding it in DHT?)"/>
<node CREATED="1475904773204" ID="ID_504053581" MODIFIED="1475904799784" TEXT="So I&apos;d think not, or the chain can lose its ability to prove itself"/>
</node>
</node>
<node CREATED="1475904810404" ID="ID_214862856" MODIFIED="1475904822416" TEXT="what is DHT data outside of chains for?">
<node CREATED="1475904823380" ID="ID_434543869" MODIFIED="1475904826952" TEXT="Can chains refer to it?"/>
</node>
<node CREATED="1475904920348" ID="ID_1720833595" MODIFIED="1475904940952" TEXT="How does autonomic determine how much security is necessary?"/>
<node CREATED="1475904942412" ID="ID_124903628" MODIFIED="1475904950360" TEXT="Will autonomic have oracles?"/>
<node CREATED="1475905020548" FOLDED="true" ID="ID_194143690" MODIFIED="1480463559545" TEXT="Suppose agoras exists">
<node CREATED="1475905235861" ID="ID_305165960" MODIFIED="1475905239808" TEXT="doublespend prevention">
<node CREATED="1475905026524" ID="ID_1441673511" MODIFIED="1475905037848" TEXT="It wants to assure there is enough security to prevent a double spend">
<node CREATED="1475905165604" ID="ID_1648166287" MODIFIED="1475905169712" TEXT="Does it need an oracle for this?"/>
</node>
<node CREATED="1475905038196" ID="ID_1226000526" MODIFIED="1475905212744" TEXT="So it must create a bounty for root chain miners">
<node CREATED="1475905216228" ID="ID_1205082546" MODIFIED="1475905224144" TEXT="regardless if there are any transactions or not">
<node CREATED="1475905225685" ID="ID_153783730" MODIFIED="1475905232096" TEXT="(otherwise someone could doublespend it)"/>
</node>
</node>
</node>
<node CREATED="1475905259516" ID="ID_483130457" MODIFIED="1475905290968" TEXT="can another sub-context use agoras?">
<node CREATED="1475905339940" ID="ID_712961151" MODIFIED="1475905432000" TEXT="If a sub context makes a condition that if a miner puts its statement into the root block, then it pays the miner agoras">
<node CREATED="1475905434237" ID="ID_1115358735" MODIFIED="1475905442168" TEXT="Where would this logic reside?">
<node CREATED="1475905448684" ID="ID_1498315843" MODIFIED="1475905454504" TEXT="I would think it would have to be Agoras"/>
</node>
</node>
</node>
<node CREATED="1475905313404" ID="ID_231707872" MODIFIED="1475905318792" TEXT="mining client">
<node CREATED="1475905319884" ID="ID_540706016" MODIFIED="1475905334488" TEXT="Accepts any number of statements to put into a block from any number of sub-contexts"/>
</node>
<node CREATED="1475905574236" ID="ID_1249527104" MODIFIED="1475905580736" TEXT="suppose agoras pays miners">
<node CREATED="1475905581796" ID="ID_632115791" MODIFIED="1475905586376" TEXT="but that is sub-contracting">
<node CREATED="1475905588500" ID="ID_656739864" MODIFIED="1475905597256" TEXT="why not just mine root itself?">
<node CREATED="1475905617900" ID="ID_450564643" MODIFIED="1475905629976" TEXT="It&apos;s owned by multiple people, so there has to be a tax"/>
</node>
</node>
</node>
<node CREATED="1475905667252" ID="ID_403321707" MODIFIED="1475905673088" TEXT="If there is a competing currency">
<node CREATED="1475905676428" ID="ID_1264150078" MODIFIED="1475905691704" TEXT="it could pay the miners very little">
<node CREATED="1475905692684" ID="ID_459311144" MODIFIED="1475905702296" TEXT="since any additional profit is still profit, the miners would pick it up"/>
</node>
</node>
</node>
<node CREATED="1475904052380" FOLDED="true" ID="ID_659314297" MODIFIED="1480463560568" TEXT="presale coin">
<node CREATED="1475904055867" ID="ID_1963607431" MODIFIED="1475904080215" TEXT="give hmc, stoopkid money as it comes in"/>
<node CREATED="1475904080651" ID="ID_1251281817" MODIFIED="1475904087944" TEXT="hold the money given in trust"/>
<node CREATED="1475904089940" ID="ID_1390722189" MODIFIED="1475904099024" TEXT="include agoras somehow?"/>
<node CREATED="1475904123220" ID="ID_1074531714" MODIFIED="1475904128487" TEXT="how about a presale coin where">
<node CREATED="1475904129691" ID="ID_1777299135" MODIFIED="1475904139344" TEXT="a) agoras tokens currently held are valid"/>
<node CREATED="1475904139828" ID="ID_255344368" MODIFIED="1475904171848" TEXT="b) an additional round of financing is held for $100K">
<node CREATED="1475904172851" ID="ID_914533269" MODIFIED="1475904178071" TEXT="The $100K is put into a trust"/>
<node CREATED="1475904178468" ID="ID_878529869" MODIFIED="1475904196575" TEXT="50% is paid directly to HMC, stoopkid by me"/>
<node CREATED="1475904198315" ID="ID_253243495" MODIFIED="1475904264224" TEXT="When autonomic is released, then $100K is released to me"/>
</node>
<node CREATED="1475904509116" ID="ID_859082386" MODIFIED="1475904512536" TEXT="too much risk">
<node CREATED="1475904513684" ID="ID_1484474598" MODIFIED="1475904531656" TEXT="who&apos;s to say someone won&apos;t come up with a diferrent presale token"/>
</node>
</node>
</node>
<node CREATED="1477381706115" FOLDED="true" ID="ID_49593062" MODIFIED="1497252363554" TEXT="barendregt">
<node CREATED="1477381842033" ID="ID_1816500353" MODIFIED="1477381844660" TEXT="my notation">
<node CREATED="1477381849392" ID="ID_1899274073" MODIFIED="1477381851659" TEXT="&gt;=">
<node CREATED="1477381852795" ID="ID_602262458" MODIFIED="1477381854746" TEXT="|-"/>
</node>
<node CREATED="1477381864919" ID="ID_567140708" MODIFIED="1477381865749" TEXT="G">
<node CREATED="1477381867168" ID="ID_719561870" MODIFIED="1477381867955" TEXT="gamma">
<node CREATED="1477382568351" ID="ID_632442534" MODIFIED="1477382570973" TEXT="Generic context"/>
<node CREATED="1477382573637" ID="ID_24628638" MODIFIED="1477382573637" TEXT=""/>
</node>
</node>
<node CREATED="1477382144124" ID="ID_1170907622" MODIFIED="1477382145244" TEXT="-&gt;&gt;">
<node CREATED="1477382146440" ID="ID_997949709" MODIFIED="1477382149078" TEXT="beta reduces">
<node CREATED="1477382150232" ID="ID_813721200" MODIFIED="1477382154669" TEXT="beta is the application rule">
<node CREATED="1477382156068" ID="ID_966365813" MODIFIED="1477382169592" TEXT="\y.y x -&gt;&gt; x"/>
</node>
</node>
</node>
<node CREATED="1477382172776" ID="ID_1203260957" MODIFIED="1477382175984" TEXT="\y">
<node CREATED="1477382177724" ID="ID_1002772006" MODIFIED="1477382182586" TEXT="lambda &lt;var&gt;"/>
</node>
<node CREATED="1477382443670" ID="ID_1167700299" MODIFIED="1477382446524" TEXT="&lt;&gt;">
<node CREATED="1477382447333" ID="ID_1881835136" MODIFIED="1477382448840" TEXT="empty context"/>
</node>
<node CREATED="1477386599191" ID="ID_819658612" MODIFIED="1477386600956" TEXT="/\">
<node CREATED="1477386602253" ID="ID_987840937" MODIFIED="1477386604826" TEXT="Pi"/>
<node CREATED="1477386605371" ID="ID_236889897" MODIFIED="1477386609539" TEXT="Cartesian product"/>
</node>
<node CREATED="1477453090632" ID="ID_157883356" MODIFIED="1477453091322" TEXT="T">
<node CREATED="1477453092480" ID="ID_1011619774" MODIFIED="1477453113833" TEXT="term">
<node CREATED="1477453122227" ID="ID_960958175" MODIFIED="1477453162529" TEXT="T=x|c|T T|\x:T.T|/\x:T.T"/>
</node>
</node>
<node CREATED="1477467462731" ID="ID_888628928" MODIFIED="1477467463244" TEXT="/">
<node CREATED="1477467464590" ID="ID_304050036" MODIFIED="1477467505198">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      abbreviation for horizontal line,
    </p>
    <p>
      
    </p>
    <p>
      Ex. a / b&#160;&#160;=&#160;&#160;&#160;&#160;&#160;&#160;&#160;a
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;------------
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;b
    </p>
  </body>
</html></richcontent>
</node>
</node>
</node>
<node CREATED="1477384651809" ID="ID_1092037418" MODIFIED="1477384676945" TEXT="(\x.x):(A-&gt;A)">
<node CREATED="1477384678424" ID="ID_1736913909" MODIFIED="1477384690453" TEXT="lambda x dot x in A arrow A">
<node CREATED="1477384691685" ID="ID_1751211961" MODIFIED="1477384718217" TEXT="for each &quot;a&quot; in A, the application (\x.x)a is also in A"/>
</node>
</node>
<node CREATED="1477384745985" ID="ID_609966426" MODIFIED="1477384750621" TEXT="simple typed lambda calc">
<node CREATED="1477384751601" ID="ID_911728007" MODIFIED="1477384759994" TEXT="I(a) = \x:A.x">
<node CREATED="1477384761470" ID="ID_23380994" MODIFIED="1477384774706" TEXT="There are many version of the identity function"/>
<node CREATED="1477384786538" ID="ID_190327471" MODIFIED="1477384788259" TEXT="Type is">
<node CREATED="1477384789188" ID="ID_896445826" MODIFIED="1477384792608" TEXT="A-&gt;A"/>
</node>
</node>
</node>
<node CREATED="1477384990522" ID="ID_634921160" MODIFIED="1477385003147" TEXT="If A and B are types, then A-&gt;B is the type of functions from A to B">
<node CREATED="1477385006153" ID="ID_1795967306" MODIFIED="1477385023935" TEXT="if F:(A-&gt;B) and x:A then (Fx):B"/>
</node>
<node CREATED="1477385049060" FOLDED="true" ID="ID_933847041" MODIFIED="1497252363545" TEXT="dependencies">
<node CREATED="1477385056570" ID="ID_1719098967" MODIFIED="1477385058161" TEXT="terms on terms">
<node CREATED="1477385059422" ID="ID_1618817361" MODIFIED="1477385061073" TEXT="FM">
<node CREATED="1477385064889" ID="ID_821576980" MODIFIED="1477385072522" TEXT="where F and M are *any* term"/>
</node>
</node>
<node CREATED="1477385087986" ID="ID_1296092638" MODIFIED="1477385095522" TEXT="terms on types">
<node CREATED="1477385100434" ID="ID_615268374" MODIFIED="1477385129448" TEXT="I(a) = \x:A.x">
<node CREATED="1477385130607" ID="ID_862351464" MODIFIED="1477385137920" TEXT="I(a) depends on A"/>
<node CREATED="1477385140407" ID="ID_839181306" MODIFIED="1477385150044" TEXT="I is the identity function in simple tlc"/>
<node CREATED="1477385234272" ID="ID_933353391" MODIFIED="1477385251089" TEXT="note that I believe here that I can be *any* term and this applies"/>
</node>
</node>
<node CREATED="1477385257086" ID="ID_125203843" MODIFIED="1477385259226" TEXT="types on terms">
<node CREATED="1477385269478" ID="ID_1180144971" MODIFIED="1477385284880" TEXT="A(n)-&gt;B">
<node CREATED="1477385291006" ID="ID_104320309" MODIFIED="1477385295901" TEXT="n is a natural number"/>
<node CREATED="1477385296219" ID="ID_147773979" MODIFIED="1477385297772" TEXT="defined by">
<node CREATED="1477385299234" ID="ID_1770925654" MODIFIED="1477385547857" TEXT="A(0)-&gt;B = B"/>
<node CREATED="1477385304131" ID="ID_1274409320" MODIFIED="1477385325210" TEXT="A(n+1)-&gt;B=A-&gt;(A(n)-&gt;B)"/>
</node>
<node CREATED="1477385525358" ID="ID_837931650" MODIFIED="1477385541952" TEXT="note here that each A(x) is a different type"/>
<node CREATED="1477385608352" ID="ID_620038809" MODIFIED="1477385613119" TEXT="Also note the equality">
<node CREATED="1477385615441" ID="ID_1148993621" MODIFIED="1477385624857" TEXT="ie A(0)-&gt;B = B">
<node CREATED="1477385626945" ID="ID_1728611171" MODIFIED="1477385640429" TEXT="So if you have a &quot;A(0)-&gt;B&quot; you can get a &quot;B&quot;, and vice versa"/>
<node CREATED="1477385670442" ID="ID_1974541448" MODIFIED="1477385672416" TEXT="But how??"/>
</node>
</node>
</node>
<node CREATED="1477385681473" ID="ID_753355112" MODIFIED="1477385694048" TEXT="A(n+1)-&gt;B=A-&gt;(A(n)-&gt;B)"/>
</node>
<node CREATED="1477385746612" ID="ID_162536474" MODIFIED="1477385752913" TEXT="type on type">
<node CREATED="1477385754165" ID="ID_975652286" MODIFIED="1477385756032" TEXT="A-&gt;A">
<node CREATED="1477385757808" ID="ID_1262595848" MODIFIED="1477385760700" TEXT="for A a given type"/>
<node CREATED="1477385768328" ID="ID_1580699633" MODIFIED="1477385774457" TEXT="A-&gt;A depends on A"/>
</node>
</node>
<node CREATED="1477385869874" ID="ID_1639057898" MODIFIED="1477391552547" TEXT="depends means">
<node CREATED="1477385878698" ID="ID_1231911184" MODIFIED="1477385961500" TEXT="One item needs the other in order to evaluate it?"/>
<node CREATED="1477385962336" ID="ID_141421761" MODIFIED="1477386127098" TEXT="You are unable to describe one item without mentioning the other item?"/>
</node>
<node CREATED="1477391553181" ID="ID_915760154" MODIFIED="1477392878752" TEXT="terms">
<node CREATED="1477392878734" ID="ID_82513167" MODIFIED="1477392881171" TEXT="can be defined as">
<node CREATED="1477391561869" ID="ID_102135969" MODIFIED="1477391591951" TEXT="The item on the left side of a &apos;:&apos; in a statement?"/>
</node>
</node>
<node CREATED="1477391593575" ID="ID_807264674" MODIFIED="1477392872272" TEXT="types">
<node CREATED="1477392872250" ID="ID_995708360" MODIFIED="1477392934821" TEXT="can be defined as">
<node CREATED="1477391596304" ID="ID_517584821" MODIFIED="1477391608862" TEXT="The item on the right side of a &apos;:&apos; in a statement?"/>
</node>
<node CREATED="1477392935416" ID="ID_1283120976" MODIFIED="1477392938274" TEXT="construction of types"/>
</node>
</node>
<node CREATED="1477386407348" ID="ID_1773936733" MODIFIED="1477386987616" TEXT="Pi(cartesian product)">
<node CREATED="1477386987589" ID="ID_699468878" MODIFIED="1477386989989" TEXT="rational">
<node CREATED="1477386775715" ID="ID_1105907936" MODIFIED="1477386806332" TEXT="Suppose that for each a:A, a type B(a) is given, and that there is an element b(a):B(a)">
<node CREATED="1477386809236" ID="ID_1243990653" MODIFIED="1477386837032" TEXT="The idea here is to imagine there many free variables, desiginated by b(a)"/>
</node>
<node CREATED="1477387061346" ID="ID_968800242" MODIFIED="1477387070585" TEXT="\a:A.b(a)"/>
<node CREATED="1477386840113" ID="ID_1836641070" MODIFIED="1477386975286" TEXT="The above should have as type the cartesian product">
<node CREATED="1477386851691" ID="ID_1838886809" MODIFIED="1477386858540" TEXT="/\a:A.B(a)"/>
<node CREATED="1477386871068" ID="ID_1730021031" MODIFIED="1477386918088" TEXT="(Since we cannot just write: A-&gt;B as the type anymore, because there is no B, but instead, a series of B(a) types)"/>
</node>
</node>
<node CREATED="1477386418410" ID="ID_1058310916" MODIFIED="1477386569817" TEXT="Instead of A-&gt;B">
<node CREATED="1477386571029" ID="ID_1832240435" MODIFIED="1477386595109" TEXT="we have /\a:A.B"/>
<node CREATED="1477386999060" ID="ID_292800540" MODIFIED="1477387014847" TEXT="A-&gt;B becomes just an expression involving /\"/>
<node CREATED="1477387276354" ID="ID_1657418801" MODIFIED="1477387354266" TEXT="ie (A-&gt;B) is definitively equal to /\a:A.B where a is not a variable within B">
<node CREATED="1477387318670" ID="ID_61290306" MODIFIED="1477387335065" TEXT="informally this can be expressed by B(A)"/>
</node>
</node>
<node CREATED="1477387784809" ID="ID_630552047" MODIFIED="1477387807958" TEXT="similar to the product of equal factors equaling a power">
<node CREATED="1477387808970" ID="ID_879774596" MODIFIED="1477387864000" TEXT="In general math, cartesian product where i goes from 1 to N of b(i), where b(i) = b for all values (for 1&lt;=i&lt;=n), is the same as b to the power of n"/>
</node>
</node>
<node CREATED="1477394117609" FOLDED="true" ID="ID_647195938" MODIFIED="1477468176387" TEXT="Formal introductions of types">
<node CREATED="1477394012189" ID="ID_843607273" MODIFIED="1477394140962" TEXT="&quot;-&gt;&quot; (and Pi)">
<node CREATED="1477394021861" ID="ID_697071197" MODIFIED="1477394027931" TEXT="can exist in lambda!"/>
<node CREATED="1477394028959" ID="ID_1417331073" MODIFIED="1477394040311" TEXT="\n:Nat.A(n)-&gt;B">
<node CREATED="1477394048235" ID="ID_852553645" MODIFIED="1477394170702" TEXT="Actually means that this function is returning a type"/>
<node CREATED="1477394147050" ID="ID_713990733" MODIFIED="1477394154331" TEXT="Type of this function is: Nat -&gt; *">
<node CREATED="1477394160330" ID="ID_1292527422" MODIFIED="1477394167012" TEXT="ie, given a Nat, returns a type"/>
</node>
<node CREATED="1477394237707" ID="ID_906207295" MODIFIED="1477394321349" TEXT="This is types depending on terms because given &quot;a&quot; that goes on the left of a statement, it returns &quot;b&quot; that goes on the right of statement where a:?. In other words, a:b becomes valid"/>
</node>
</node>
</node>
<node CREATED="1477398186433" ID="ID_1193593393" MODIFIED="1477473743670" TEXT="Axioms not listed in main section????">
<node CREATED="1477398198827" ID="ID_1600229711" MODIFIED="1477398215153" TEXT="A-&gt;B for any A and any B is a *"/>
<node CREATED="1477453040064" ID="ID_1515833892" MODIFIED="1477453042675" TEXT="*">
<node CREATED="1477398215630" ID="ID_202495272" MODIFIED="1477398225970" TEXT="*-&gt;* is a []"/>
<node CREATED="1477398226668" ID="ID_1243821175" MODIFIED="1477398235852" TEXT="A-&gt;* is a [], too"/>
<node CREATED="1477453045018" ID="ID_1131046802" MODIFIED="1477453064412" TEXT="Basically anytime a /\ returns a *, it&apos;s type is []"/>
</node>
</node>
<node CREATED="1477465371446" ID="ID_1334786561" MODIFIED="1477468177231" TEXT="Definitions">
<node CREATED="1477453092480" ID="ID_951401209" MODIFIED="1477465553359" TEXT="term">
<node CREATED="1477453122227" ID="ID_624573509" MODIFIED="1477453162529" TEXT="T=x|c|T T|\x:T.T|/\x:T.T">
<node CREATED="1477465561842" ID="ID_280592030" MODIFIED="1477465580801" TEXT="x is the category of variables"/>
<node CREATED="1477465570439" ID="ID_182972473" MODIFIED="1477465577467" TEXT="c is the category of constants"/>
</node>
</node>
<node CREATED="1477465373760" ID="ID_1257302171" MODIFIED="1477465407873" TEXT="Statement if of the form A:B, with A, B in T">
<node CREATED="1477465407855" ID="ID_1090202742" MODIFIED="1477465409216" TEXT="A:B">
<node CREATED="1477465395914" ID="ID_234970032" MODIFIED="1477465398373" TEXT="A is Subject"/>
<node CREATED="1477465399034" ID="ID_624918383" MODIFIED="1477465405176" TEXT="B is Predicate"/>
</node>
</node>
<node CREATED="1477465521162" ID="ID_1352406680" MODIFIED="1477468179400" TEXT="Declaration">
<node CREATED="1477465527728" ID="ID_71228048" MODIFIED="1477465529481" TEXT="x:A">
<node CREATED="1477465530379" ID="ID_665895190" MODIFIED="1477465532233" TEXT="A is a term"/>
<node CREATED="1477465532477" ID="ID_347445019" MODIFIED="1477465535397" TEXT="x is a variable"/>
</node>
</node>
<node CREATED="1477465820551" ID="ID_298374998" MODIFIED="1477465838960" TEXT="pseudo context G">
<node CREATED="1477465824165" ID="ID_793027678" MODIFIED="1477465857578" TEXT="A finite ordered sequence of declarations, all with distinct subjects"/>
<node CREATED="1477468101235" ID="ID_294965537" MODIFIED="1477468128632" TEXT="This is different than a context, since a pseudo context is defined to be a concrete list of declarations">
<node CREATED="1477468129660" ID="ID_732296781" MODIFIED="1477468144932" TEXT="A context is not defined to be any structure in particular, but yields certain facts"/>
</node>
</node>
</node>
<node CREATED="1477382429521" ID="ID_1816153461" MODIFIED="1477468181676" TEXT="General axiom and rules">
<node CREATED="1477382434485" ID="ID_1670446623" MODIFIED="1477382441921" TEXT="&lt;&gt; &gt;= *:[]"/>
<node CREATED="1477465281399" ID="ID_1383597087" MODIFIED="1477465345459" TEXT="(\x:A.B)C(-&gt;b)B[x:=C]"/>
<node CREATED="1477467826600" ID="ID_419095962" MODIFIED="1477473719627" TEXT="Rules">
<node CREATED="1477467836186" ID="ID_1908736839" MODIFIED="1477467867317" TEXT="For all rules, let s,s1,s2 range over S, where is is {*,[])"/>
<node CREATED="1477467960907" ID="ID_720652765" MODIFIED="1477467968294" TEXT="General">
<node CREATED="1477467972275" ID="ID_1732825247" MODIFIED="1477467976753" TEXT="Applies to all systems"/>
<node CREATED="1477467114371" ID="ID_484447599" MODIFIED="1477467118289" TEXT="Start rule">
<node CREATED="1477467120108" ID="ID_1202894991" MODIFIED="1477467150331" TEXT="G &gt;= A:s / G,x:A &gt;= x:A"/>
<node CREATED="1477467152177" ID="ID_1226645735" MODIFIED="1477467884587" TEXT="Given that gamma yields A:s (ie. A is a * or a []), if we add x:A to the statements in the set of gamma, then gamma yields that x:A (as long as x isn&apos;t already part of the statements of gamma)"/>
</node>
<node CREATED="1477467217677" ID="ID_1560596969" MODIFIED="1477467221197" TEXT="Weakening rule">
<node CREATED="1477467246034" ID="ID_569043450" MODIFIED="1477467279395" TEXT="(G &gt;= A:B  G &gt;= C:s) / (G,x:C &gt;= A:B)"/>
<node CREATED="1477467351016" ID="ID_280029153" MODIFIED="1477467716598" TEXT="If gamma already yields that A:B is true, if we add x:C to the set of statements representing Gamma, A:B is still true. x must not already be in Gamma for this to apply"/>
</node>
<node CREATED="1477467431259" ID="ID_181104667" MODIFIED="1477467970789" TEXT="Application rule">
<node CREATED="1477467435562" ID="ID_1068455316" MODIFIED="1477467549316">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      G &gt;= F:(/\x:A.B)&#160;&#160;G &gt;= a:A&#160;&#160;&#160;
    </p>
    <p>
      -----------------------------------
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;G &gt;= (Fa):B[x:=a]
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1477467552124" ID="ID_1100282448" MODIFIED="1477467694158">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      If gamma yields there is a Pi called F that given an object of type A returns an object of type B, and there is an object &quot;a&quot; of type A,
    </p>
    <p>
      then
    </p>
    <p>
      Gamma yields that F applied to &quot;a&quot; is of type B where the free var x is replaced with a
    </p>
  </body>
</html></richcontent>
</node>
</node>
<node CREATED="1477467735778" ID="ID_917958255" MODIFIED="1477467738307" TEXT="Conversion rule">
<node CREATED="1477467739923" ID="ID_1133106040" MODIFIED="1477467781261">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      G &gt;= A:B&#160;&#160;&#160;G &gt;= B':s&#160;&#160;&#160;&#160;&#160;B(=b) B'
    </p>
    <p>
      -------------------------------------------
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;G &gt;= A:B'
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1477467795823" ID="ID_1197110212" MODIFIED="1477467944344" TEXT="If gamma yields B&apos; is a star or a box, and Gamma yields that A is of type B, and B and B&apos; are beta equivalent, than Gamma yields that A is of type B&apos;"/>
</node>
</node>
<node CREATED="1477467979077" ID="ID_1060673133" MODIFIED="1477467981216" TEXT="Specific">
<node CREATED="1477467982698" ID="ID_1371403211" MODIFIED="1477467996272" TEXT="/\ rule">
<node CREATED="1477468002183" ID="ID_951524308" MODIFIED="1477468084921">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      &#160;&#160;&#160;G &gt;= A:s1&#160;&#160;&#160;G,x:A &gt;= B:s2
    </p>
    <p>
      ---------------------------------------
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;G &gt;= (/\x:A.B):s2
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1477472048380" ID="ID_1304763062" MODIFIED="1477472097109" TEXT="This can be interpreted differently depending on s1,s2">
<node CREATED="1477472098673" ID="ID_1887029953" MODIFIED="1477472100666" TEXT="*,*">
<node CREATED="1477472968192" ID="ID_829470059" MODIFIED="1477473059750" TEXT="This means that if you have a way of yielding a type B, given the presence of an object "/>
<node CREATED="1477484269614" ID="ID_990139961" MODIFIED="1477484316333" TEXT="For example, imagine that given an x:Q-&gt;P and a q:Q, there must be a function that given a (Q-&gt;P) it can generate a P. "/>
</node>
</node>
</node>
<node CREATED="1477484321263" ID="ID_306864688" MODIFIED="1477484326938" TEXT="\ rule">
<node CREATED="1477484339159" ID="ID_23585776" MODIFIED="1477484343079" TEXT="...."/>
<node CREATED="1477484343801" ID="ID_867156629" MODIFIED="1477484347925" TEXT="*,*">
<node CREATED="1477484349670" ID="ID_1470750859" MODIFIED="1477484449428" TEXT="Means that, given a type A, and that if you add a var of type A to a given gamma, and it produces an object of type B, and given the object of type A, a type B is also derived, then you can derive a function from \x:A.b:(/\x:A.B)"/>
</node>
</node>
</node>
</node>
<node CREATED="1477484714640" ID="ID_236511921" MODIFIED="1477484794870" TEXT="Notational conventions">
<node CREATED="1477484721553" ID="ID_1966604860" MODIFIED="1477484750355" TEXT="A-&gt;B === (/\x:A.B) with x not in Free vars of B"/>
<node CREATED="1477484753667" ID="ID_1825795700" MODIFIED="1477484774753" TEXT="G &gt;= A:B:C means G &gt;= A:B and G &gt;= B:C"/>
</node>
<node CREATED="1477484841445" ID="ID_1439431352" MODIFIED="1477484846272" TEXT="Definitions">
<node CREATED="1477484847767" ID="ID_1909286024" MODIFIED="1477484853284" TEXT="G &gt;= A:B">
<node CREATED="1477484855478" ID="ID_172944049" MODIFIED="1477484870573" TEXT="Gamma is a context, A and B are terms"/>
</node>
<node CREATED="1477484878262" ID="ID_664843331" MODIFIED="1477484889819" TEXT="G &gt;= A:B:*">
<node CREATED="1477484893141" ID="ID_891318744" MODIFIED="1477484896206" TEXT="A is object"/>
<node CREATED="1477484896568" ID="ID_495470289" MODIFIED="1477484898278" TEXT="B is type"/>
</node>
<node CREATED="1477484899341" ID="ID_111087074" MODIFIED="1477484906887" TEXT="G &gt;= A:B:[]">
<node CREATED="1477484910323" ID="ID_546881861" MODIFIED="1477484918364" TEXT="A is constructor">
<node CREATED="1477484925934" ID="ID_1240106790" MODIFIED="1477484931535" TEXT="(type constructor I believe)"/>
</node>
<node CREATED="1477484918851" ID="ID_1795174400" MODIFIED="1477484921970" TEXT="B is kind"/>
</node>
</node>
</node>
<node CREATED="1477381763681" ID="ID_680144306" MODIFIED="1477381813944" TEXT="G &gt;= A:B">
<node CREATED="1477381823920" ID="ID_1591958469" MODIFIED="1477381840198" TEXT="A and B are terms and G is a context"/>
</node>
<node CREATED="1477381860598" ID="ID_241805232" MODIFIED="1477381876768" TEXT="G &gt;= A:B:*">
<node CREATED="1477381878874" ID="ID_1600756261" MODIFIED="1477381884196" TEXT="A is object, B is type"/>
</node>
<node CREATED="1477381885618" ID="ID_1864899757" MODIFIED="1477381891421" TEXT="G &gt;= A:B:[]">
<node CREATED="1477381893276" ID="ID_655740725" MODIFIED="1477382039226" TEXT="A is constructor, B is a kind"/>
</node>
<node CREATED="1477381951453" FOLDED="true" ID="ID_370520808" MODIFIED="1477382104514" TEXT="all types B are constructors. This is the only overlap between terms">
<node CREATED="1477381990294" ID="ID_1753817817" MODIFIED="1477381995775" TEXT="B:*:[]">
<node CREATED="1477381997175" ID="ID_981636167" MODIFIED="1477382020883" TEXT="Therefore, in the above, A:B:* means B is a type"/>
<node CREATED="1477382021238" ID="ID_41333229" MODIFIED="1477382035133" TEXT="and A:B:[] means B is a constructor"/>
</node>
</node>
<node CREATED="1477382105832" ID="ID_1614425500" MODIFIED="1477382137006" TEXT="G &gt;= A:B &amp; A -&gt;&gt;(beta reduces) A&apos; =&gt; G &gt;= A&apos;:B"/>
<node CREATED="1477382217878" ID="ID_560812822" MODIFIED="1477382353668" TEXT="G &gt;= A:B imply A and B are strongly normalizing ">
<node CREATED="1477382232841" ID="ID_498298010" MODIFIED="1477382236464" TEXT="All beta reductions terminate"/>
</node>
<node CREATED="1477382356160" ID="ID_1504115856" MODIFIED="1477382380357" TEXT="If G&gt;=A:B and G&gt;=A:B&apos; then B (beta equals) B&apos;"/>
</node>
<node CREATED="1477114577238" FOLDED="true" ID="ID_1381280979" MODIFIED="1503360042176" TEXT="lennart">
<node CREATED="1477181078957" ID="ID_369498126" MODIFIED="1477181082350" TEXT="lambda untyped">
<node CREATED="1477181083343" ID="ID_1337106957" MODIFIED="1477181087311" TEXT="Identity Function">
<node CREATED="1477181088253" ID="ID_1997990896" MODIFIED="1477181095892" TEXT="Lam &quot;x&quot; $ Var &quot;x&quot; "/>
</node>
</node>
<node CREATED="1477137489243" FOLDED="true" ID="ID_1314878807" MODIFIED="1503360041430" TEXT="lambda simple types">
<node CREATED="1477137505444" ID="ID_669022596" MODIFIED="1477137510456" TEXT="lam is the only thing with a type">
<node CREATED="1477137511413" ID="ID_1007781717" MODIFIED="1477137519989" TEXT="The type describes the argument *only*"/>
<node CREATED="1477137520358" ID="ID_1435769587" MODIFIED="1477137551309" TEXT="A expr that is a lambda is represented by an arrow">
<node CREATED="1477137552427" ID="ID_23197204" MODIFIED="1477137560725" TEXT="B-&gt;B"/>
</node>
<node CREATED="1477137578358" ID="ID_852990158" MODIFIED="1477137614301" TEXT="So when we type check a lambda, we figure out what it will return based on the variables, and then return (arg)-&gt;(ret type)"/>
</node>
<node CREATED="1477183101468" ID="ID_837348966" MODIFIED="1477183104109" TEXT="Structure">
<node CREATED="1477183105155" ID="ID_1585669263" MODIFIED="1477183286253">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      data Expr
    </p>
    <p>
      &#160;&#160;&#160;&#160;= Var Sym
    </p>
    <p>
      &#160;&#160;&#160;&#160;| App Expr Expr
    </p>
    <p>
      &#160;&#160;&#160;&#160;| Lam Sym Type Expr
    </p>
    <p>
      &#160;&#160;&#160;&#160;deriving (Eq, Read, Show)
    </p>
    <p>
      
    </p>
    <p>
      data Type = Base | Int | Char | Arrow Type Type
    </p>
    <p>
      &#160;&#160;&#160;&#160;deriving (Eq, Read, Show)
    </p>
  </body>
</html></richcontent>
</node>
</node>
<node CREATED="1477183067221" ID="ID_741265574" MODIFIED="1477183071990" TEXT="Identity Function">
<node CREATED="1477183184723" ID="ID_1652134973" MODIFIED="1477183251365" TEXT="(Lam &quot;x&quot; Base (Var &quot;x&quot;))"/>
<node CREATED="1477183260671" ID="ID_895032338" MODIFIED="1477183299176" TEXT="Note that needs a different function for each possible type"/>
<node CREATED="1477183338410" ID="ID_1200535513" MODIFIED="1477183339393" TEXT="Type">
<node CREATED="1477183340666" ID="ID_1576555459" MODIFIED="1477183351198" TEXT="(Arrow Base Base)"/>
</node>
</node>
<node CREATED="1477183306611" ID="ID_1036296187" MODIFIED="1477183405971" TEXT="Nat Function">
<node CREATED="1477183405951" ID="ID_1797647443" MODIFIED="1477183408119" TEXT="Nat zero">
<node CREATED="1477183355457" ID="ID_1583378640" MODIFIED="1477183421968" TEXT="(Lam &quot;f&quot; (Arrow Base Base) (Lam &quot;z&quot; Base (Var &quot;z&quot;)))"/>
</node>
<node CREATED="1477183424163" ID="ID_1375291582" MODIFIED="1477183425703" TEXT="Nat one">
<node CREATED="1477183427033" ID="ID_1923931679" MODIFIED="1477183440497" TEXT="(Lam &quot;f&quot; (Arrow Base Base) (Lam &quot;z&quot; Base (App f (Var &quot;z&quot;)))) "/>
</node>
<node CREATED="1477183444808" ID="ID_1978300001" MODIFIED="1477183446639" TEXT="Type">
<node CREATED="1477183448155" ID="ID_520766498" MODIFIED="1477183472853" TEXT="(Arrow (Arrow Base Base (Arrow Base Base)))"/>
</node>
</node>
<node CREATED="1477183477663" ID="ID_1821268968" MODIFIED="1477183480891" TEXT="Typecheck">
<node CREATED="1477183482038" ID="ID_1715404149" MODIFIED="1477183513361" TEXT="(Lam &lt;sym&gt; &lt;type&gt; &lt;expr&gt;)">
<node CREATED="1477183514659" ID="ID_1920565875" MODIFIED="1477183689506" TEXT="(Arrow &lt;type&gt; (typecheck &lt;expr&gt;))"/>
<node CREATED="1477183690853" ID="ID_577346819" MODIFIED="1477183702525" TEXT="Add &lt;sym&gt; to freevars table with type &lt;type&gt;"/>
</node>
<node CREATED="1477183703725" ID="ID_462846513" MODIFIED="1477183713016" TEXT="(Var &lt;sym&gt;)">
<node CREATED="1477183714410" ID="ID_971122952" MODIFIED="1477183722635" TEXT="Lookup &lt;sym&gt; in freevars">
<node CREATED="1477183726440" ID="ID_1202255185" MODIFIED="1477183730792" TEXT="Return corresponding &lt;type&gt;"/>
</node>
</node>
<node CREATED="1477183731777" ID="ID_886008485" MODIFIED="1477185490383" TEXT="(App &lt;expr1&gt; &lt;expr2&gt;)">
<node CREATED="1477185591683" ID="ID_1806808938" MODIFIED="1477185616752" TEXT="let t1 = (typecheck &lt;expr1&gt;) "/>
<node CREATED="1477185591683" ID="ID_1513865143" MODIFIED="1477185631350" TEXT="let t2 = (typecheck &lt;expr2&gt;) "/>
<node CREATED="1477183741461" ID="ID_1659248354" MODIFIED="1477185685108" TEXT="t1 must be (Arrow &lt;at1&gt; &lt;rt1&gt;)"/>
<node CREATED="1477185493109" ID="ID_267219450" MODIFIED="1477185693685" TEXT="t2 must equal &lt;at1&gt;"/>
<node CREATED="1477185693988" ID="ID_1602293037" MODIFIED="1477185698070" TEXT="return &lt;rt1&gt;"/>
</node>
</node>
</node>
<node CREATED="1477180959129" FOLDED="true" ID="ID_1871136436" MODIFIED="1503360040230" TEXT="lambda polymorphic">
<node CREATED="1477183083866" ID="ID_1032483406" MODIFIED="1477183085748" TEXT="Structure">
<node CREATED="1477183086982" ID="ID_888847051" MODIFIED="1477183097214">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      data Expr
    </p>
    <p>
      &#160;&#160;&#160;&#160;= Var Sym
    </p>
    <p>
      &#160;&#160;&#160;&#160;| App Expr Expr
    </p>
    <p>
      &#160;&#160;&#160;&#160;| Lam Sym Type Expr
    </p>
    <p>
      &#160;&#160;&#160;&#160;| TLam Sym Kind Expr
    </p>
    <p>
      &#160;&#160;&#160;&#160;| TApp Expr Type
    </p>
    <p>
      &#160;&#160;&#160;&#160;deriving (Eq, Read, Show)
    </p>
    <p>
      data Type
    </p>
    <p>
      &#160;&#160;&#160;&#160;= Arrow Type Type
    </p>
    <p>
      &#160;&#160;&#160;&#160;| Base
    </p>
    <p>
      &#160;&#160;&#160;&#160;| TVar Sym
    </p>
    <p>
      &#160;&#160;&#160;&#160;deriving (Eq, Read, Show)
    </p>
    <p>
      data Kind
    </p>
    <p>
      &#160;&#160;&#160;&#160;= KArrow Type Type
    </p>
    <p>
      &#160;&#160;&#160;&#160;| Star
    </p>
    <p>
      &#160;&#160;&#160;&#160;deriving (Eq, Read, Show)
    </p>
  </body>
</html></richcontent>
</node>
</node>
<node CREATED="1477180963345" ID="ID_1625745945" MODIFIED="1477180967230" TEXT="Identity Function">
<node CREATED="1477180977484" ID="ID_1005633478" MODIFIED="1477191754615" TEXT="(TLam &quot;t&quot; Star (Lam $ TVar &quot;t&quot; $ &quot;x&quot; $ Var &quot;x&quot;)) "/>
<node CREATED="1477181112626" ID="ID_932366639" MODIFIED="1477181117278" TEXT="Pass in a type and a value">
<node CREATED="1477181121890" ID="ID_1004024961" MODIFIED="1477181124965" TEXT="ex">
<node CREATED="1477191912609" ID="ID_1872227015" MODIFIED="1477192160596" TEXT="(App (TApp (TLam &quot;t&quot; Star (Lam $ TVar &quot;t&quot; $ &quot;x&quot; $ Var &quot;x&quot;)) Base) (Var &quot;y&quot;)) ">
<node CREATED="1477192166683" ID="ID_105865781" MODIFIED="1477192329746" TEXT="note for simplicity, assume Var &quot;y&quot; exists and is of type Base"/>
</node>
<node CREATED="1477192184122" ID="ID_1247477972" MODIFIED="1477192267753">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      (App (Lam Base &quot;x&quot; $ Var &quot;x&quot;)) (Var &quot;y&quot;))
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1477192294878" ID="ID_1007782684" MODIFIED="1477192314941">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      (Var &quot;y&quot;)
    </p>
  </body>
</html></richcontent>
</node>
</node>
</node>
</node>
<node CREATED="1477181589492" ID="ID_1257870170" MODIFIED="1477181590886" TEXT="Nat zero">
<node CREATED="1477181591889" ID="ID_1689734928" MODIFIED="1477181595365" TEXT="(TLam &quot;a&quot; Star (Lam &quot;f&quot; (Arrow $ TVar &quot;a&quot; $ TVar &quot;a&quot;) (Lam &quot;z&quot; $ TVar &quot;a&quot; (Var &quot;z&quot;)))) "/>
</node>
<node CREATED="1477181960488" ID="ID_911306277" MODIFIED="1477181961824" TEXT="Nat one">
<node CREATED="1477181963216" ID="ID_443449495" MODIFIED="1477182024524" TEXT="(TLam &quot;a&quot; Star (Lam &quot;f&quot; (Arrow $ TVar &quot;a&quot; $ TVar &quot;a&quot;) (Lam &quot;z&quot; $ TVar &quot;a&quot; (App $ Var &quot;f&quot; $ Var &quot;z&quot;)))"/>
</node>
<node CREATED="1477181352189" ID="ID_1910594013" MODIFIED="1477181948572" TEXT="Nat type">
<node CREATED="1477181632745" ID="ID_1626359261" MODIFIED="1477192663940">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      (TLam &quot;a&quot; Star (Arrow
    </p>
    <p>
      &#160;&#160;(Arrow (Arrow $ TVar &quot;a&quot; $ TVar &quot;a&quot;) (TVar &quot;a&quot;))
    </p>
    <p>
      &#160;&#160;(TVar &quot;a&quot;)
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;)
    </p>
    <p>
      )
    </p>
  </body>
</html></richcontent>
<node CREATED="1477182420357" ID="ID_1518829402" MODIFIED="1477182433749" TEXT="forall a:* ((a-&gt;a)-&gt;a-&gt;a)"/>
</node>
</node>
<node CREATED="1477182440005" ID="ID_1843883576" MODIFIED="1477183498096" TEXT="typecheck">
<node CREATED="1477182456637" ID="ID_1977407898" MODIFIED="1477182481620" TEXT="(TLam &lt;sym&gt; &lt;kind&gt; &lt;expr&gt;)">
<node CREATED="1477182483237" ID="ID_331497887" MODIFIED="1477182518508" TEXT="(TLam &lt;sym&gt; &lt;kind&gt; (typecheck &lt;expr&gt;))"/>
</node>
<node CREATED="1477191316495" ID="ID_1607113914" MODIFIED="1477191366491" TEXT="(TApp &lt;exp&gt; &lt;type&gt;)">
<node CREATED="1477191555863" ID="ID_679420628" MODIFIED="1477192389475" TEXT="exp must be a TLam"/>
<node CREATED="1477192389950" ID="ID_1580577791" MODIFIED="1477192535294" TEXT="run TLam against type, must return a type"/>
</node>
<node CREATED="1477182520427" ID="ID_42965494" MODIFIED="1477182553149" TEXT="(Lam &lt;sym&gt; &lt;type-expr&gt; &lt;expr&gt;)">
<node CREATED="1477182554241" ID="ID_1004447470" MODIFIED="1477182755043" TEXT="(Arrow &lt;type-expr&gt; (typecheck &lt;expr&gt;))"/>
<node CREATED="1477182796879" ID="ID_960414360" MODIFIED="1477182819214" TEXT="(also add &lt;sym&gt; to table of freevars, with type &lt;type-expr&gt;)"/>
</node>
<node CREATED="1477182759696" ID="ID_545864030" MODIFIED="1477182766795" TEXT="(Var &lt;sym&gt;)">
<node CREATED="1477182767841" ID="ID_667087357" MODIFIED="1477182833190" TEXT="Lookup &lt;sym&gt; in freevars table and get associated type"/>
</node>
<node CREATED="1477182875806" ID="ID_336795005" MODIFIED="1477182904262" TEXT="(App &lt;expr1&gt; &lt;expr2&gt;)">
<node CREATED="1477182905586" ID="ID_590650682" MODIFIED="1477182910670" TEXT="Typecheck expr1"/>
<node CREATED="1477182911383" ID="ID_1812540846" MODIFIED="1477182914936" TEXT="Typecheck expr2"/>
<node CREATED="1477182915228" ID="ID_1586466773" MODIFIED="1477182928090" TEXT="expr1 must be in the form (t1)-&gt;(t2)">
<node CREATED="1477182928970" ID="ID_1758503463" MODIFIED="1477182946174" TEXT="ie (Arrow &lt;t1&gt; &lt;t2&gt;)"/>
</node>
<node CREATED="1477182947263" ID="ID_420520658" MODIFIED="1477183042086" TEXT="expr2 type must equal &lt;t1&gt;">
<node CREATED="1477182960600" ID="ID_1639095225" MODIFIED="1477183025324" TEXT="Otherwise user error (apply incorrect argument to lambda, ex an Bool applied to plus)"/>
</node>
<node CREATED="1477183029573" ID="ID_1381393619" MODIFIED="1477183036681" TEXT="Return &lt;t2&gt;"/>
</node>
</node>
</node>
<node CREATED="1477114582233" FOLDED="true" ID="ID_876058750" MODIFIED="1503360038771" TEXT="lambda-cube">
<node CREATED="1477281133428" ID="ID_1478032622" MODIFIED="1477281137593" TEXT="code">
<node CREATED="1477114737082" ID="ID_1326332317" MODIFIED="1477304460864" TEXT="Files">
<node CREATED="1477114587730" ID="ID_666884489" MODIFIED="1477114637622" TEXT="Files are wrapped in">
<node CREATED="1477114638550" ID="ID_1927210484" MODIFIED="1477114666774" TEXT="let &lt;file contents&gt; in &lt;exp&gt;">
<node CREATED="1477114668817" ID="ID_1805053205" MODIFIED="1477114675601" TEXT="&lt;exp&gt; is hardcoded to">
<node CREATED="1477114676316" ID="ID_1416789754" MODIFIED="1477114688171" TEXT="&quot;\\ (a::*) -&gt; a&quot;"/>
</node>
</node>
</node>
<node CREATED="1477114739539" ID="ID_1935796557" MODIFIED="1477114748429" TEXT="Each line corresponds to a &quot;let&quot; entry">
<node CREATED="1477114752640" ID="ID_841044807" MODIFIED="1477114759469" TEXT="which means the start of a function"/>
</node>
<node CREATED="1477114771654" ID="ID_996623683" MODIFIED="1477114772734" TEXT="Let">
<node CREATED="1477114773862" ID="ID_1581700227" MODIFIED="1477114780377" TEXT="There are two types of let statements">
<node CREATED="1477114780983" ID="ID_1352690324" MODIFIED="1477114788787" TEXT="&lt;var&gt; :: &lt;type&gt;"/>
<node CREATED="1477114789161" ID="ID_949625452" MODIFIED="1477116826010" TEXT="&lt;var&gt; &lt;args&gt; = &lt;value&gt;"/>
</node>
<node CREATED="1477114893393" ID="ID_1215498466" MODIFIED="1477114897898" TEXT="This pair corresponds to">
<node CREATED="1477114902500" ID="ID_1396112154" MODIFIED="1477116846100" TEXT="(\\ &lt;var&gt; :: &lt;type&gt; &lt;expr&gt;) \\&lt;args&gt; &lt;value&gt;"/>
<node CREATED="1477114960022" ID="ID_1719259307" MODIFIED="1477114967749" TEXT="where &lt;expr&gt; is the rest of the file"/>
</node>
<node CREATED="1477116861520" ID="ID_1664023865" MODIFIED="1477116864519" TEXT="Ex.">
<node CREATED="1477116865571" ID="ID_346995972" MODIFIED="1477116866042" TEXT="id">
<node CREATED="1477116866877" ID="ID_175348288" MODIFIED="1477119583775" TEXT="(\id:(\a.\b-&gt;\b).id &lt;expr&gt;)(\a:* x:a.x)">
<node CREATED="1477117275878" ID="ID_1670902851" MODIFIED="1477117277210" TEXT="or">
<node CREATED="1477117280662" ID="ID_1809626194" MODIFIED="1477117298605" TEXT="(\id:(\a.a-&gt;a).id &lt;data&gt;)(\a:* \x:a.x)"/>
</node>
<node CREATED="1477117304178" ID="ID_278637067" MODIFIED="1477117316902" TEXT="Note that to run this function you need to supply a type, and a value">
<node CREATED="1477117321207" ID="ID_1375871476" MODIFIED="1477117323030" TEXT="ex">
<node CREATED="1477117323870" ID="ID_265939159" MODIFIED="1477117349639" TEXT="(\a:* x:a.x) Int 5"/>
<node CREATED="1477117352489" ID="ID_636812166" MODIFIED="1477117362374" TEXT="\x:Int.x 5"/>
<node CREATED="1477117362806" ID="ID_1792570901" MODIFIED="1477117367965" TEXT="5"/>
</node>
</node>
</node>
<node CREATED="1477117156049" ID="ID_761691541" MODIFIED="1477117175379" TEXT="Let ">
<node CREATED="1477117176642" ID="ID_1286200548" MODIFIED="1477117177857" TEXT="id :: a-&gt;a">
<node CREATED="1477117179104" ID="ID_1687859383" MODIFIED="1477117224497" TEXT="or &quot;id :: forall (a::*) a-&gt;a&quot;"/>
</node>
<node CREATED="1477117237602" ID="ID_231292950" MODIFIED="1477117264232" TEXT="id a x = x"/>
</node>
</node>
<node CREATED="1477119139775" ID="ID_458568443" MODIFIED="1477119141107" TEXT="Bool">
<node CREATED="1477119143401" ID="ID_19496001" MODIFIED="1477119148009" TEXT="Let">
<node CREATED="1477119149318" ID="ID_1419050125" MODIFIED="1477119365688" TEXT="Bool :: *"/>
<node CREATED="1477119153952" ID="ID_1167608289" MODIFIED="1477119243989" TEXT="Bool = forall (boolT::*) . boolT-&gt;boolT-&gt;boolT;">
<font NAME="SansSerif" SIZE="12"/>
</node>
</node>
<node CREATED="1477119328271" ID="ID_766020169" MODIFIED="1477119534487" TEXT="(\\ Bool::*. &lt;expr&gt;)(\Bool::*.\boolT::.\\booT"/>
</node>
<node CREATED="1477119127357" ID="ID_1175595498" MODIFIED="1477119129671" TEXT="List">
<node CREATED="1477119131811" ID="ID_1062104253" MODIFIED="1477119131811" TEXT=""/>
</node>
<node CREATED="1477117379372" ID="ID_625270078" MODIFIED="1477117382293" TEXT="Either">
<node CREATED="1477117384710" ID="ID_1974073072" MODIFIED="1477117387494" TEXT="Let">
<node CREATED="1477117388604" ID="ID_815453401" MODIFIED="1477117411736" TEXT="Either :: * -&gt; * -&gt; *"/>
<node CREATED="1477117420181" ID="ID_903380730" MODIFIED="1477117436710" TEXT="Either a b = "/>
</node>
</node>
</node>
</node>
</node>
<node CREATED="1477193039676" ID="ID_270876666" MODIFIED="1477193042139" TEXT="expr">
<node CREATED="1477193043398" ID="ID_1526974836" MODIFIED="1477193049259" TEXT="data Expr&#xa;    = Var Sym&#xa;    | App Expr Expr&#xa;    | Lam Sym Type Expr&#xa;    | Pi  Sym Type Type&#xa;    | Kind Kinds&#xa;    deriving (Eq, Read, Show)&#xa;type Type = Expr&#xa;&#xa;data Kinds = Star | Box deriving (Eq, Read, Show)"/>
</node>
</node>
<node CREATED="1477281147589" ID="ID_784853842" MODIFIED="1477281153540" TEXT="basically standardizes operations">
<node CREATED="1477281154763" ID="ID_1141177395" MODIFIED="1477281182851" TEXT="places a bunch of different type languages in different categories based on the arguments they allow for these specialized operations">
<node CREATED="1477281183896" ID="ID_240967168" MODIFIED="1477281187458" TEXT="either kind or star"/>
<node CREATED="1477281187974" ID="ID_300930365" MODIFIED="1477281192133" TEXT="star == type"/>
<node CREATED="1477281194153" ID="ID_1939675174" MODIFIED="1477281199031" TEXT="kind is a type of types">
<node CREATED="1477281250761" ID="ID_1004099246" MODIFIED="1477281254925" TEXT="represented by a box"/>
</node>
</node>
</node>
<node CREATED="1477282323778" ID="ID_357582714" MODIFIED="1477282338709" TEXT="ex falso sequitur quodlibet">
<node CREATED="1477282340338" ID="ID_402523793" MODIFIED="1477282347835" TEXT="Property of the explosion of false"/>
<node CREATED="1477282348261" ID="ID_614460342" MODIFIED="1477282361631" TEXT="&lt;&gt;:-*:[]">
<node CREATED="1477282364106" ID="ID_454262492" MODIFIED="1477282376869" TEXT="Is this saying that all types exist in an empty context?"/>
</node>
<node CREATED="1477282381063" ID="ID_1954194606" MODIFIED="1477301408377">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      &lt;&gt; |- *:[]
    </p>
    <p>
      
    </p>
    <p>
      \a:0.a : 0-&gt;0
    </p>
    <p>
      
    </p>
    <p>
      (\b:*(\a:0.a)b):(forall b:*.0-&gt;b)
    </p>
    <p>
      
    </p>
    <p>
      (forall b:*.0-&gt;b)
    </p>
    <p>
      
    </p>
    <p>
      0 = (forall a:*.a)
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1477304170191" ID="ID_1942904977" MODIFIED="1477304180498" TEXT="Is 0 the empty set or the full set?"/>
</node>
<node CREATED="1477301421678" ID="ID_7765649" MODIFIED="1477301426409" TEXT="forall">
<node CREATED="1477301427376" ID="ID_573971094" MODIFIED="1477301436116" TEXT="it seems as if &quot;forall&quot; implies a set"/>
<node CREATED="1477301436482" ID="ID_521646164" MODIFIED="1477301456604" TEXT="Also when a set contains a forall, then it saying, add all elements within the forall into the set"/>
</node>
</node>
</node>
<node CREATED="1477308537216" ID="ID_1680921308" MODIFIED="1490410956918" TEXT="irc">
<node CREATED="1476966604452" FOLDED="true" ID="ID_355970099" MODIFIED="1497252356800" TEXT="general">
<node CREATED="1476966606717" ID="ID_1058588269" MODIFIED="1476966612697" TEXT="redfish64: so i take it you didn&apos;t read about effect types or heap tracing yet?                XD "/>
<node CREATED="1477352256980" ID="ID_581115542" MODIFIED="1477352258788" TEXT="03:26 &lt; HMCa&gt; you&apos;re bordering on what i&apos;d call &quot;my biggest open question&quot; wrt impl detail... 03:27 &lt; HMCa&gt; and that is &quot;if, and to what degree, coinduction and effect types would be necessary for a stable and *useful* genesis&quot; "/>
<node CREATED="1477358836410" ID="ID_1218368955" MODIFIED="1477358860243" TEXT="03:58 &lt; HMCa&gt; so simpler, easier is a great tutorial explanation of the cube, but doesn&apos;t really reflect the spectrum of dtlc impl as well&#xa;03:58 &lt; HMCa&gt; but&#xa;03:59 &lt; HMCa&gt; i can give you a simpler-easier-alike that does just that... its come up before in #zen&#xa;03:59 &lt; HMCa&gt; that is dolio&apos;s pts tower example&#xa;03:59 &lt; HMCa&gt; hub.darcs.net/dolio/pts&#xa;03:59 &lt; HMCa&gt; and&#xa;03:59 &lt; HMCa&gt; hub.darcs.net/dolio/upts&#xa;0"/>
<node CREATED="1477706536451" ID="ID_548650899" MODIFIED="1477706594655">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      04:54 &lt; HMCa&gt; you sort of answered your own question more than I think you
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;realize
    </p>
    <p>
      04:56 &lt; HMCa&gt; contexts can look at other contexts' data, but cannonly ever
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;consistently *interpret* anything meaningful from that data
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;relative to some mutual ancestor chain
    </p>
    <p>
      04:56 &lt; HMCa&gt; there are 2 problems there
    </p>
    <p>
      04:57 &lt; HMCa&gt; first, to know which chain would be considered globally
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;longest by any potential &quot;fourth context&quot;
    </p>
    <p>
      04:58 &lt; HMCa&gt; and second, to know which background theory stack to use
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&quot;below set 0&quot; in that contexts interpretation
    </p>
    <p>
      05:00 &lt; HMCa&gt; (interpretation of that context depend on which chain is
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;longest on the parent, so you need to also ibterpret the
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;parent... which depends on which chain is longest in the
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;grandparent, which.... up to the common ancestor (maybe root)
    </p>
    <p>
      05:00 &lt; HMCa&gt; so &quot;ignoring root&quot; you can see all chains, yes
    </p>
    <p>
      05:00 &lt; HMCa&gt; but the problem with this is that you see *all* chains ;-)
    </p>
    <p>
      05:01 &lt; HMCa&gt; to &quot;identify and ignore&quot; the ones that are definitionally
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;short forks, you need that common ancestry logic...
    </p>
    <p>
      05:02 &lt; HMCa&gt; which, if nothing else in common, means root. ;-)
    </p>
    <p>
      05:02 &lt; HMCa&gt; *this* is what motivates mining on root...
    </p>
    <p>
      05:04 &lt; HMCa&gt; it is not just about &quot;verification against the higher chains&quot;
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;but also, and more importantly, about verification against
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;sibling chains!
    </p>
    <p>
      05:05 &lt; HMCa&gt; our virtual universes must consistently cosimulate, even in
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;the presence of conflicting alternate realities being
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;considered for some of them. :-D
    </p>
  </body>
</html></richcontent>
</node>
</node>
<node CREATED="1477879774330" FOLDED="true" ID="ID_378080981" MODIFIED="1478308689460" TEXT="sub-contexts">
<node CREATED="1477879778619" ID="ID_525301166" MODIFIED="1477879780012" TEXT="examples">
<node CREATED="1477879780924" ID="ID_273995728" MODIFIED="1477880477593">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      09:38 &lt; redfish64&gt; Hey, wondering if you had some examples in mind of
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;typical applications for sub-contexts in that could be
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;implemented in the near future once autonomic is complete
    </p>
    <p>
      09:39 &lt; stoopkid&gt; well, smart contracts is probably the big one
    </p>
    <p>
      09:46 &lt; redfish64&gt; Any others? I'm looking to figure out what AutoNomic can
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;do that other systems can't. Provable smart contracts
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;could be done on other coins, just prove them out of
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;band, right?
    </p>
    <p>
      09:48 &lt; stoopkid&gt; DAOs generally
    </p>
    <p>
      09:49 &lt; stoopkid&gt; code-for-money markets, decentralized supercomputing,
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;decentralized theorem proving, generalized decentralized
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;database with consensus on state, proof-lookup database
    </p>
    <p>
      09:49 &lt; stoopkid&gt; code-lookup database
    </p>
    <p>
      09:49 &lt; stoopkid&gt; secure software distribution
    </p>
    <p>
      09:50 &lt; redfish64&gt; excellent, this is what I was looking for. I want to find
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;some examples so I can try and model how they would work
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;in AutoNomic
    </p>
    <p>
      09:53 &lt; stoopkid&gt; i've been thinking about crowd-sourced databases and
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;incentivizing DHTs a bit
    </p>
    <p>
      09:54 &lt; stoopkid&gt; i realized that you don't necessarily need storage
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;incentivization if your data comes in small chunks and
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;everybody just stores what's relevant to them
    </p>
    <p>
      09:55 &lt; stoopkid&gt; so what i wanna work on post-genesis is what i was working
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;on before i found the tau project: a way to generalize
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;wikipedia to arbitrary content &amp; interface
    </p>
    <p>
      09:58 &lt; stoopkid&gt; but that's essentially a trivial use-case of tau
    </p>
    <p>
      10:00 &lt; stoopkid&gt; so then i'd want to look at expanding the subcontext
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;management tools to get to the point that subcontexts
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;further out on the fringe can be &quot;arbitrary software&quot;,
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;like real-time video games, just to give an example of the
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;kinda functionality i aim for this to support
    </p>
  </body>
</html></richcontent>
</node>
</node>
</node>
</node>
<node CREATED="1477879760749" FOLDED="true" ID="ID_1425686767" MODIFIED="1495697521274" TEXT="governance">
<node CREATED="1477884676626" ID="ID_17853211" MODIFIED="1477884679633" TEXT="idea">
<node CREATED="1477884680571" ID="ID_1124127846" MODIFIED="1477884682969" TEXT="PoW voting">
<node CREATED="1477884684082" ID="ID_1103065988" MODIFIED="1477884698880" TEXT="when mining a block, a miner gets to vote on proposals"/>
<node CREATED="1477884702697" ID="ID_351167538" MODIFIED="1477884940476" TEXT="in general a non-vote means to vote no (probably what people would implement their clients to do anyway)">
<node CREATED="1477907968350" ID="ID_1440394688" MODIFIED="1477907974426" TEXT="or we could make a neutral vote as well"/>
</node>
<node CREATED="1477884739182" ID="ID_446457964" MODIFIED="1477884748986" TEXT="votes can be positive or negative for a proposal"/>
<node CREATED="1477884722426" ID="ID_1849556584" MODIFIED="1477884755222" TEXT="after a proposal gets x many positive votes the change occurs"/>
<node CREATED="1477907950275" ID="ID_540603748" MODIFIED="1477907961164" TEXT="x many negative votes and the proposal fails to pass"/>
<node CREATED="1477884826922" ID="ID_1024136835" MODIFIED="1477884852387" TEXT="I don&apos;t think proposals could be reversable, in some cases, so a proposal that did get in would be permanent."/>
<node CREATED="1477907979151" ID="ID_1533728815" MODIFIED="1477907987144" TEXT="this should eliminate having to fork"/>
</node>
<node CREATED="1477907988418" ID="ID_1619479629" MODIFIED="1477907994160" TEXT="fork emergency">
<node CREATED="1477907995430" ID="ID_1135930480" MODIFIED="1477908014470" TEXT="if the new code has an emergency problem, we&apos;ll want to fork right away">
<node CREATED="1477908059496" ID="ID_26894203" MODIFIED="1477908064766" TEXT="It can even fork in the past"/>
</node>
<node CREATED="1477908014948" ID="ID_953565662" MODIFIED="1477908025716" TEXT="a miner can create an emergency fork.">
<node CREATED="1477908028415" ID="ID_418339181" MODIFIED="1477908056972" TEXT="In general other miner clients will be set to ignore this emergency fork unless enabled"/>
</node>
<node CREATED="1477908079145" ID="ID_1815152427" MODIFIED="1477908098222" TEXT="In this way, a change can occur immediately, getting around the delay caused by the Pow Voting scheme"/>
</node>
</node>
</node>
<node CREATED="1477914570378" FOLDED="true" ID="ID_739037137" MODIFIED="1490410955298" TEXT="hott">
<node CREATED="1477914574559" ID="ID_1278774491" MODIFIED="1477914583349" TEXT="there is no complete implementation of hott">
<node CREATED="1477914621025" ID="ID_355960662" MODIFIED="1477914657900">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      http://cstheory.stackexchange.com/questions/21199/what-parts-of-homotopy-type-theory-are-not-possible-in-agda-or-coq
    </p>
  </body>
</html></richcontent>
</node>
</node>
<node CREATED="1477914585256" ID="ID_1370315206" MODIFIED="1477914666361" TEXT="supposed to be easier to prove things">
<node CREATED="1477914668360" ID="ID_1154439946" MODIFIED="1477914679000" TEXT="819 hott stanford lecture "/>
</node>
<node CREATED="1477914589331" ID="ID_848120086" MODIFIED="1477914592600" TEXT="what about ott?">
<node CREATED="1477914595838" ID="ID_924500121" MODIFIED="1477914617086">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      https://pigworker.wordpress.com/2015/04/01/warming-up-to-homotopy-type-theory/
    </p>
  </body>
</html></richcontent>
</node>
</node>
</node>
<node CREATED="1478578196555" FOLDED="true" ID="ID_602144672" MODIFIED="1490410954223" TEXT="idris">
<node CREATED="1478578198755" ID="ID_702632447" MODIFIED="1478578200424" TEXT="Fin">
<node CREATED="1478578201401" ID="ID_44390231" MODIFIED="1478578223400">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      data Fin : Nat -&gt; Type where
    </p>
    <p>
      FZ : Fin (S k)
    </p>
    <p>
      FS : Fin k -&gt; Fin (S k)
    </p>
  </body>
</html></richcontent>
<node CREATED="1478578224817" ID="ID_684946584" MODIFIED="1478578272905" TEXT="The idea is that it&apos;s impossible to create a FS (FS ... (FZ)) statement where the number reprsented by (FS (FS ... FZ)) is higher than (S k)"/>
<node CREATED="1478578274687" ID="ID_121507004" MODIFIED="1478578288394" TEXT="This means that we can&apos;t create an index in a set where the index is higher than the set"/>
<node CREATED="1478578288674" ID="ID_1370974552" MODIFIED="1478578304496" TEXT="Then you can take k and use it in another object, such as vector">
<node CREATED="1478578310314" ID="ID_1314365172" MODIFIED="1478578326528">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      index : Fin n -&gt; Vect n a -&gt; a
    </p>
    <p>
      index FZ
    </p>
    <p>
      (x :: xs) = x
    </p>
    <p>
      index (FS k) (x :: xs) = index k xs
    </p>
  </body>
</html></richcontent>
</node>
</node>
</node>
</node>
</node>
<node CREATED="1476833527659" FOLDED="true" ID="ID_1449979034" MODIFIED="1490410961294" TEXT="10/19/16">
<node CREATED="1476833540530" ID="ID_48281273" MODIFIED="1476833549861" TEXT="data">
<node CREATED="1476833551034" ID="ID_1015424848" MODIFIED="1476833562917" TEXT="data is stored into the chain">
<node CREATED="1476833568586" ID="ID_1791312970" MODIFIED="1476833579701" TEXT="a structure is like a product (except that its ordered)">
<node CREATED="1476833580819" ID="ID_1147374850" MODIFIED="1476833582885" TEXT="to pay a user">
<node CREATED="1476833583817" ID="ID_527508062" MODIFIED="1476833606181" TEXT="(user)*(user)*(amount)*(date) etc...">
<node CREATED="1476833607314" ID="ID_552592270" MODIFIED="1476833634837" TEXT="Can mean a request to pay from one user to the other user on a particular date for a particular amount"/>
</node>
</node>
</node>
<node CREATED="1476833819690" ID="ID_1052604160" MODIFIED="1476833826405" TEXT="what then is not stored in the chain?">
<node CREATED="1476833853009" ID="ID_676638898" MODIFIED="1476833860621" TEXT="It probably depends on the importance of data"/>
<node CREATED="1476833892833" ID="ID_1560758949" MODIFIED="1476833896885" TEXT="But likely a lot of it"/>
</node>
<node CREATED="1476833928369" ID="ID_1579249003" MODIFIED="1476833948405" TEXT="When a transaction record appears, how does that fire off the rules that are affected by it?">
<node CREATED="1476833958026" ID="ID_307567000" MODIFIED="1476833973317" TEXT="Maybe they don&apos;t get fired off"/>
</node>
<node CREATED="1476838266994" ID="ID_838095050" MODIFIED="1476838277245" TEXT="How are all records of type X queried?"/>
<node CREATED="1476838296826" ID="ID_130638074" MODIFIED="1476838315343" TEXT="what does storing in the chain do for data?">
<node CREATED="1476838316282" ID="ID_1075762747" MODIFIED="1476838329574" TEXT="I would assume that it would mean the data would be guaranteed to be available">
<node CREATED="1476838333634" ID="ID_687758247" MODIFIED="1476838336199" TEXT="But is this true?">
<node CREATED="1476838337674" ID="ID_685179409" MODIFIED="1476838370374" TEXT="To mine, you need the hash of the latest block, that&apos;s it, I would suppose">
<node CREATED="1476838375890" ID="ID_1505007424" MODIFIED="1476838405558" TEXT="Well, you still need to verify the proof that it is valid, which would mean going back to previous blocks in the chain"/>
<node CREATED="1476838415626" ID="ID_282663390" MODIFIED="1476838441861" TEXT="Unless you didn&apos;t always check every proof"/>
<node CREATED="1476838465258" ID="ID_752854444" MODIFIED="1476838477557" TEXT="You may be able to close out blocks that you don&apos;t need anymore"/>
<node CREATED="1476838477842" ID="ID_14410385" MODIFIED="1476838489789" TEXT="Or even strip out information that is not relevant to mining"/>
<node CREATED="1476838490035" ID="ID_916111468" MODIFIED="1476838503374" TEXT="Therefore data on the chain does not necessarily mean that it will always be capturable"/>
</node>
</node>
</node>
</node>
<node CREATED="1476838569986" ID="ID_1165660268" MODIFIED="1476838598270" TEXT="It seems that there has to be an incentive for storing data then, since even on chain data can&apos;t really be trusted to be there if it&apos;s not directly involved in mining"/>
<node CREATED="1476838837930" ID="ID_1314341123" MODIFIED="1476838845999" TEXT="Somehow you could require all data be kept">
<node CREATED="1476838848618" ID="ID_1426159180" MODIFIED="1476838856478" TEXT="Some pseudo random index into data"/>
</node>
<node CREATED="1476839341314" ID="ID_1466721684" MODIFIED="1476839342134" TEXT="ex">
<node CREATED="1476839343378" ID="ID_1984975528" MODIFIED="1476839346406" TEXT="Mary wants her balance">
<node CREATED="1476839347562" ID="ID_611564432" MODIFIED="1476839370014" TEXT="Transactions could be stored within AutoNomic">
<node CREATED="1476839355355" ID="ID_367593817" MODIFIED="1476839362902" TEXT="Mary could query all transactions and find her balance"/>
</node>
<node CREATED="1476839374523" ID="ID_889226592" MODIFIED="1476839381990" TEXT="The balance itself could be stored within AutoNomic"/>
<node CREATED="1476839435563" ID="ID_1438229077" MODIFIED="1476839457510" TEXT="How could one prove they got all transactions involving Mary&apos;s balance to compute this."/>
</node>
</node>
</node>
</node>
<node CREATED="1476847467930" ID="ID_1728071187" MODIFIED="1476847471862" TEXT="short term chains">
<node CREATED="1476847474034" ID="ID_1273687293" MODIFIED="1476847477166" TEXT="how are these created?">
<node CREATED="1476847498226" ID="ID_1969990485" MODIFIED="1476847499342" TEXT="rules">
<node CREATED="1476847500170" ID="ID_403332559" MODIFIED="1476847506214" TEXT="but what rules?"/>
</node>
</node>
<node CREATED="1476847488314" ID="ID_1226079231" MODIFIED="1476847492718" TEXT="what are they used for?">
<node CREATED="1476847493018" ID="ID_639361737" MODIFIED="1476847495678" TEXT=" microtransactions"/>
</node>
</node>
<node CREATED="1476860159514" ID="ID_489616987" MODIFIED="1476860240214" TEXT="how does the chain separate private and public data?">
<node CREATED="1476860316618" ID="ID_1127596334" MODIFIED="1476860405033" TEXT="If a user loads a random context that he doesn&apos;t trust very much, how a"/>
</node>
<node CREATED="1477308532634" ID="ID_758419312" MODIFIED="1478308695827" TEXT="questions">
<node CREATED="1477308545306" ID="ID_237183945" MODIFIED="1477308586180" TEXT="Why is False described as everything in lambda cube paper, page 8? http://www.diku.dk/hjemmesider/ansatte/henglein/papers/barendregt1991.pdf"/>
<node CREATED="1477308664473" ID="ID_676710406" MODIFIED="1477308688457" TEXT="How to represent a product? A pair is represented by: * -&gt; * -&gt; *"/>
<node CREATED="1477308691759" ID="ID_1726676208" MODIFIED="1477308716898" TEXT="A product seems to need to take a variable number of args">
<node CREATED="1477308720174" ID="ID_1155768832" MODIFIED="1477308733569" TEXT="I suppose a recursive definition (such as Nat) would work?"/>
</node>
<node CREATED="1477309083647" ID="ID_645364996" MODIFIED="1477309120214" TEXT="Data">
<node CREATED="1477309120190" ID="ID_1015739584" MODIFIED="1477309120933" TEXT="?">
<node CREATED="1477309086503" ID="ID_318451142" MODIFIED="1477309098882" TEXT="Data is represented as functions">
<node CREATED="1477309100685" ID="ID_391947707" MODIFIED="1477309110093" TEXT="In the same general way Nats are represented"/>
<node CREATED="1477309128205" ID="ID_1372624920" MODIFIED="1477309130000" TEXT="Ex. ">
<node CREATED="1477309221561" ID="ID_1058139377" MODIFIED="1477309228608" TEXT="\ (natT :: *) (zero :: natT) (succ :: natT-&gt;natT) -&gt; zero   :: Nat"/>
</node>
<node CREATED="1477310576721" ID="ID_1110115232" MODIFIED="1477310597767" TEXT="But, technically, all Nats from 0 to infinity are now in the universe">
<node CREATED="1477310600341" ID="ID_654114293" MODIFIED="1477310626609" TEXT="So it wouldn&apos;t work for data, because not all possible data items exist in the universe"/>
</node>
</node>
<node CREATED="1477310565723" ID="ID_1738069889" MODIFIED="1477310573738" TEXT="Data is represented as axioms">
<node CREATED="1477310661912" ID="ID_1072166695" MODIFIED="1477310685132" TEXT="Very clear, just like &lt;&gt;:-*:[]">
<node CREATED="1477310686600" ID="ID_1485794762" MODIFIED="1477310749539" TEXT="We say : (AutoNomic universe):-Person:*, john:Person"/>
</node>
</node>
<node CREATED="1477310632562" ID="ID_1628052336" MODIFIED="1477310655185" TEXT="Data is represented by assumptions (ie let)"/>
</node>
</node>
</node>
</node>
<node CREATED="1477743757577" FOLDED="true" ID="ID_19273052" MODIFIED="1478578195237" TEXT="10/29/16">
<node CREATED="1477743769710" ID="ID_120253349" MODIFIED="1477743855756" TEXT="what to do now">
<node CREATED="1477743858340" ID="ID_1923248621" MODIFIED="1477743875879" TEXT="HMC and stoopkid are designing some language">
<node CREATED="1477743882487" ID="ID_1720383031" MODIFIED="1477743891254" TEXT="it revolves around upts">
<node CREATED="1477743896396" ID="ID_668960669" MODIFIED="1477743902933" TEXT="universal pure type theory"/>
</node>
</node>
<node CREATED="1477743909835" ID="ID_1414238793" MODIFIED="1477743913320" TEXT="whats left?">
<node CREATED="1477743914445" ID="ID_1898607850" MODIFIED="1477743917096" TEXT="the outer core"/>
<node CREATED="1477743917539" ID="ID_1152979428" MODIFIED="1477743924969" TEXT="the api between the two"/>
<node CREATED="1477743935004" ID="ID_837840208" MODIFIED="1477743949004" TEXT="writing the code in one language or another"/>
<node CREATED="1477743951331" ID="ID_1349455497" MODIFIED="1477743958942" TEXT="figuring out if it will work as designed">
<node CREATED="1477744040116" ID="ID_603074103" MODIFIED="1477744046715" TEXT="so we have a root chain"/>
<node CREATED="1477744057364" ID="ID_1709356578" MODIFIED="1477744068750" TEXT="we need at least one application for anyone to mine it at all"/>
<node CREATED="1477744087637" ID="ID_136467791" MODIFIED="1477744093745" TEXT="so lets say we have one app">
<node CREATED="1477744095828" ID="ID_114567849" MODIFIED="1477744098102" TEXT="a coin app"/>
</node>
<node CREATED="1477744112558" ID="ID_423979776" MODIFIED="1477744120443" TEXT="so we have subcontext and a root context">
<node CREATED="1477744159253" ID="ID_1475757060" MODIFIED="1477744164198" TEXT="what is the network topology?">
<node CREATED="1477744165648" ID="ID_809231114" MODIFIED="1477744245454" TEXT="we have dht">
<node CREATED="1477744256912" ID="ID_1136273986" MODIFIED="1477744262462" TEXT="or something else?"/>
</node>
<node CREATED="1477744263683" ID="ID_1940202972" MODIFIED="1477744266693" TEXT="we have nodes">
<node CREATED="1477744271826" ID="ID_899262741" MODIFIED="1477744278834" TEXT="all nodes will be interested in the coin app"/>
<node CREATED="1477744282712" ID="ID_533690493" MODIFIED="1477744287281" TEXT="mining it"/>
<node CREATED="1477744289623" ID="ID_1562882115" MODIFIED="1477744293217" TEXT="why mine root?">
<node CREATED="1477744337647" ID="ID_1671949678" MODIFIED="1477744354228" TEXT="so that future contexts can use your coin?">
<node CREATED="1477744501900" ID="ID_863981623" MODIFIED="1477744522283" TEXT="probably a rule will exist to reward miners for a root peg">
<node CREATED="1477744525511" ID="ID_1743607857" MODIFIED="1477744537637" TEXT="how will this be known in the sub context?"/>
</node>
</node>
<node CREATED="1477744475159" ID="ID_747547797" MODIFIED="1477744490484" TEXT="also it will look good to the community to mine root"/>
</node>
</node>
</node>
<node CREATED="1477744122457" ID="ID_1130067885" MODIFIED="1477744126149" TEXT="what are the rules?"/>
</node>
<node CREATED="1477744554919" ID="ID_520404851" MODIFIED="1477744560549" TEXT="the data as seen by the client">
<node CREATED="1477744570373" ID="ID_1836945847" MODIFIED="1477744578534" TEXT="too much data?">
<node CREATED="1477744856925" ID="ID_308477192" MODIFIED="1477744862391" TEXT="data stored cached on disk"/>
<node CREATED="1477744863492" ID="ID_1251047433" MODIFIED="1477744868030" TEXT="in memory"/>
<node CREATED="1477744868362" ID="ID_407974280" MODIFIED="1477744872012" TEXT="on network"/>
</node>
<node CREATED="1477744579764" ID="ID_1169859294" MODIFIED="1477744588325" TEXT="data represented as statements?">
<node CREATED="1477744806704" ID="ID_246439657" MODIFIED="1477744808062" TEXT="yes"/>
</node>
<node CREATED="1477744588677" ID="ID_757984006" MODIFIED="1477744593747" TEXT="what kinds of queries are needed?">
<node CREATED="1477744765077" ID="ID_990678504" MODIFIED="1477744782413" TEXT="any tables are going to have to be separate structures, lists, sets, etc."/>
</node>
</node>
</node>
<node CREATED="1477744377620" ID="ID_385180916" MODIFIED="1477744383220" TEXT="alternative designs">
<node CREATED="1477744384936" ID="ID_210534695" MODIFIED="1477744388274" TEXT="braid design?"/>
<node CREATED="1477744388668" ID="ID_651831271" MODIFIED="1477744391126" TEXT="root coin?"/>
<node CREATED="1477744391431" ID="ID_635365659" MODIFIED="1477744395599" TEXT="PoS?"/>
</node>
</node>
</node>
</node>
<node CREATED="1479258446820" FOLDED="true" ID="ID_1741812806" MODIFIED="1480463553432" TEXT="11/16/16">
<node CREATED="1479258456885" ID="ID_870326086" MODIFIED="1479258457748" TEXT="plan">
<node CREATED="1479258460114" ID="ID_418565133" MODIFIED="1479258489666" TEXT="print out cube in regular format, ie (Lam x (Pi ...)) to get a feel for things">
<node CREATED="1479258530273" ID="ID_329602738" MODIFIED="1479258537478" TEXT="finish lambda cube html"/>
</node>
<node CREATED="1479258511922" ID="ID_825126649" MODIFIED="1479258517369" TEXT="continue reading type theory book"/>
<node CREATED="1479258517850" ID="ID_1678939638" MODIFIED="1479258523194" TEXT="read about lambda auth"/>
<node CREATED="1479429360166" ID="ID_1831026874" MODIFIED="1479429363277" TEXT="herbrand universes"/>
<node CREATED="1479429363667" ID="ID_1138960675" MODIFIED="1479429366922" TEXT="f-algebra"/>
<node CREATED="1479429367575" ID="ID_1240337066" MODIFIED="1479429371740" TEXT="typed y combinator">
<node CREATED="1479429372837" ID="ID_69827247" MODIFIED="1479429393423" TEXT="can a y combinator exist in a typed lambda functional calculus?"/>
</node>
</node>
<node CREATED="1479383374071" ID="ID_102337975" MODIFIED="1479383379371" TEXT="new autonomic code">
<node CREATED="1479383381057" ID="ID_803124336" MODIFIED="1479383382526" TEXT="requirements">
<node CREATED="1479383383706" ID="ID_984606268" MODIFIED="1479383383706" TEXT=""/>
</node>
</node>
</node>
<node CREATED="1479717914234" FOLDED="true" ID="ID_739165508" MODIFIED="1484376524065" TEXT="11/21/16">
<node CREATED="1479716427445" ID="ID_1492190587" MODIFIED="1479716429270" TEXT="questions">
<node CREATED="1479716430363" ID="ID_1965956573" MODIFIED="1479716492077" TEXT="how to represent data in autonomic?">
<node CREATED="1479716440663" ID="ID_179000226" MODIFIED="1479716453637" TEXT="a free variable?">
<node CREATED="1479716459629" ID="ID_1402439275" MODIFIED="1479716544829" TEXT="maybe surround with a lambda function, ex \User.E">
<node CREATED="1479716546064" ID="ID_760549934" MODIFIED="1479716551701" TEXT="where &quot;User&quot; is a type"/>
</node>
</node>
</node>
<node CREATED="1479716492445" ID="ID_1818992942" MODIFIED="1479716502405" TEXT="what sort of database do we need?"/>
<node CREATED="1479717996660" ID="ID_1050300267" MODIFIED="1479718006272" TEXT="how can we prove a language is what it needs to be"/>
<node CREATED="1479718006740" ID="ID_1601221969" MODIFIED="1479718010282" TEXT="what does it need to be?">
<node CREATED="1479718011115" ID="ID_363049468" MODIFIED="1479774746746" TEXT="decidable?"/>
<node CREATED="1479774736455" ID="ID_665322809" MODIFIED="1479774739434" TEXT="consistent?"/>
<node CREATED="1479774738393" ID="ID_1634093520" MODIFIED="1479774739004" TEXT="complete?"/>
</node>
</node>
<node CREATED="1479717922488" ID="ID_1312174144" MODIFIED="1479717924083" TEXT="plan">
<node CREATED="1479717924563" ID="ID_969280145" MODIFIED="1479717942222" TEXT="we need to be able to construct a basic system of what tau looks like, then move to the implementation">
<node CREATED="1479717950315" ID="ID_1340213082" MODIFIED="1479717960866" TEXT="should we use idris, coq, or agda to do this?"/>
</node>
</node>
</node>
<node CREATED="1484129473694" FOLDED="true" ID="ID_1409367945" MODIFIED="1490410962677" TEXT="1/11/17">
<node CREATED="1484129481895" ID="ID_1055800596" MODIFIED="1484129486226" TEXT="Use internal syntax">
<node CREATED="1484129487126" ID="ID_908799350" MODIFIED="1484129501017" TEXT="This is already typed checked, and possibly compiled"/>
<node CREATED="1484129503086" ID="ID_1252642337" MODIFIED="1484129516233" TEXT="It is what agda itself loads when it imports already compiled files"/>
</node>
<node CREATED="1484129527302" ID="ID_693922243" MODIFIED="1484129553354" TEXT="Agda isn&apos;t setup to support huge modules, but AN may have these">
<node CREATED="1484129554190" ID="ID_1758338576" MODIFIED="1484129585259" TEXT="We must use a thin layer to attach the interface to the tree allowing us to seemlessly lookup data required by the agda compiler"/>
<node CREATED="1484129586774" ID="ID_185153818" MODIFIED="1484129604641" TEXT="ex. suppose that we have a list of users in a module, and it is hundreds of thousands of entries">
<node CREATED="1484129606934" ID="ID_1754746920" MODIFIED="1484129632962" TEXT="We back it up with the tree, so when you type an individual name, it looks it up in the tree automatically for you, and you can then reference it"/>
</node>
</node>
<node CREATED="1484129676375" ID="ID_15713815" MODIFIED="1484129700131" TEXT="To get it in the tree, we need to be able to support updating and adding individual entries into an existing interface">
<node CREATED="1484129701894" ID="ID_653192059" MODIFIED="1484129722874" TEXT="agda doesn&apos;t have that, in all probablility, since it loads files as a unit"/>
</node>
<node CREATED="1484129754165" ID="ID_1588680448" MODIFIED="1484129775547" TEXT="Besides the above, we can just serialize the fields">
<node CREATED="1484129776533" ID="ID_1469212059" MODIFIED="1484129785074" TEXT="As long as we can get an interface out of it, were good"/>
</node>
<node CREATED="1484129811854" ID="ID_1764861164" MODIFIED="1484129813146" TEXT="v1">
<node CREATED="1484129814174" ID="ID_867049521" MODIFIED="1484129822401" TEXT="Stupid, easy tree"/>
<node CREATED="1484129822886" ID="ID_402707989" MODIFIED="1484131426155" TEXT="Instead of altering interface files, we recompile them as individual files???">
<node CREATED="1484129854614" ID="ID_229820031" MODIFIED="1484129970802" TEXT="I don&apos;t like this too much because its so far from what we need to be doing to support huge modules (altering the tree)"/>
</node>
</node>
<node CREATED="1484131665855" ID="ID_122747404" MODIFIED="1484131669402" TEXT="next steps"/>
</node>
<node CREATED="1484376544460" FOLDED="true" ID="ID_632840116" MODIFIED="1497252364458" TEXT="1/14/17">
<node CREATED="1484376614876" ID="ID_931906284" MODIFIED="1484377410418" TEXT="network">
<node CREATED="1484376624628" ID="ID_189957103" MODIFIED="1484376634543" TEXT="there are some things that need to be transmittable to all">
<node CREATED="1484376636052" ID="ID_563835031" MODIFIED="1484376722207" TEXT="verifyBlockHash">
<node CREATED="1484376641595" ID="ID_1632147652" MODIFIED="1484376653840" TEXT="This just verifies the hash of the next block, without looking at the data itself"/>
</node>
</node>
<node CREATED="1484376656467" ID="ID_275942934" MODIFIED="1484376663080" TEXT="other things are specific to a purpose">
<node CREATED="1484376664091" ID="ID_1205442718" MODIFIED="1484376669728" TEXT="transfer money from one wallet to another">
<node CREATED="1484376672068" ID="ID_765724715" MODIFIED="1484376693351" TEXT="the individual wallet holders will have proof of funds">
<node CREATED="1484376694477" ID="ID_858776287" MODIFIED="1484376701576" TEXT="a money trail from the first block"/>
</node>
</node>
<node CREATED="1484376712428" ID="ID_533267583" MODIFIED="1484376715584" TEXT="mining">
<node CREATED="1484376724091" ID="ID_1932926171" MODIFIED="1484376729103" TEXT="verifyBlock">
<node CREATED="1484376730075" ID="ID_209332615" MODIFIED="1484376753376" TEXT="Unlike verifyBlockHash, this will actually verify a proof including any changes to the tree"/>
</node>
</node>
</node>
<node CREATED="1484377410388" ID="ID_1847587902" MODIFIED="1484377411402" TEXT="data">
<node CREATED="1484376942139" ID="ID_749986749" MODIFIED="1484376950192" TEXT="altrusitic data">
<node CREATED="1484376951003" ID="ID_1706474292" MODIFIED="1484376984847" TEXT="verifyBlockHash and the block hashes themselves need to be available universally">
<node CREATED="1484376986412" ID="ID_206389622" MODIFIED="1484376996687" TEXT="This stuff is used as well, so transfer shouldn&apos;t be a problem"/>
</node>
</node>
<node CREATED="1484377001556" ID="ID_1863305817" MODIFIED="1484377040057" TEXT="other types">
<node CREATED="1484377040045" ID="ID_310010721" MODIFIED="1484377040856" TEXT="ex">
<node CREATED="1484377004436" ID="ID_1129928418" MODIFIED="1484377006839" TEXT="mining"/>
<node CREATED="1484377007228" ID="ID_875115673" MODIFIED="1484377013488" TEXT="specific application"/>
<node CREATED="1484377013868" ID="ID_760035658" MODIFIED="1484377035471" TEXT="wallets"/>
</node>
</node>
<node CREATED="1484377111699" ID="ID_39406688" MODIFIED="1484377276168" TEXT="do we really want to let clients have a verifyBlockHash?">
<node CREATED="1484377246051" ID="ID_360051891" MODIFIED="1484377265871" TEXT="sub contexts won&apos;t help with data storage"/>
<node CREATED="1484377328027" ID="ID_1132907284" MODIFIED="1484377341688" TEXT="I think this will only be used for &quot;catching up&quot; to the latest block"/>
<node CREATED="1484377347572" ID="ID_839818171" MODIFIED="1484377390584" TEXT="We don&apos;t want to make clients store a lot of data, but at the same time, the users should steer the ship. We need someone there to keep the miners honest"/>
</node>
</node>
<node CREATED="1484377403403" ID="ID_782046592" MODIFIED="1484377423143" TEXT="finding data">
<node CREATED="1484376777339" ID="ID_521504361" MODIFIED="1484376789223" TEXT="how do nodes communicate, query each other, etc.?">
<node CREATED="1484377683996" ID="ID_1524209709" MODIFIED="1484377699359" TEXT="Maybe a more basic system is needed... let the context decided"/>
<node CREATED="1484377699900" ID="ID_1872599528" MODIFIED="1484377704728" TEXT="In other words, we have">
<node CREATED="1484377705796" ID="ID_721002898" MODIFIED="1484377734184" TEXT="connect"/>
<node CREATED="1484377716340" ID="ID_962033536" MODIFIED="1484377736480" TEXT="send-msg"/>
<node CREATED="1484377736884" ID="ID_1067759021" MODIFIED="1484377738695" TEXT="disconnect"/>
</node>
<node CREATED="1484377756916" ID="ID_794015363" MODIFIED="1484377770712" TEXT="We will use monads to do this">
<node CREATED="1484377833476" ID="ID_1566140117" MODIFIED="1484377840664" TEXT="Can we use monads to do this?"/>
<node CREATED="1484377841868" ID="ID_1061315288" MODIFIED="1484377854511" TEXT="Is there a way to do something like">
<node CREATED="1484377855684" ID="ID_1832297008" MODIFIED="1484377868311" TEXT="request peerx -&gt; response"/>
<node CREATED="1484377868828" ID="ID_166326000" MODIFIED="1484377878329" TEXT="(do something with response)"/>
</node>
<node CREATED="1484378000379" ID="ID_1033218136" MODIFIED="1484378017687" TEXT="In any case we need some cookie to store contextual information with the request"/>
</node>
<node CREATED="1484378070668" ID="ID_413554550" MODIFIED="1484378082785" TEXT="I think that every request needs a response, even if a simple acknowledgement"/>
<node CREATED="1484378088324" ID="ID_577571003" MODIFIED="1484378092071" TEXT="so we have">
<node CREATED="1484378093604" ID="ID_1735945900" MODIFIED="1484378096312" TEXT="connect"/>
<node CREATED="1484378096692" ID="ID_1118678539" MODIFIED="1484378098527" TEXT="disconnect"/>
<node CREATED="1484378099197" ID="ID_842555510" MODIFIED="1484378111007" TEXT="send-request (returns response)"/>
</node>
<node CREATED="1484378112259" ID="ID_262242841" MODIFIED="1484378143296" TEXT="But how do we store data not associated with the blockchain, such as a list of peers, stats about each peer, etc?"/>
<node CREATED="1484378180852" ID="ID_155161639" MODIFIED="1484378215744" TEXT="Maybe we need a category for private data that is saved locally along with the block">
<node CREATED="1484378221964" ID="ID_1851568346" MODIFIED="1484378239591" TEXT="or maybe outside of the block chain completely"/>
</node>
<node CREATED="1484378262916" ID="ID_1568454087" MODIFIED="1484378277399" TEXT="Then we don&apos;t need send-request/receive reply "/>
<node CREATED="1484378280732" ID="ID_679295384" MODIFIED="1484378323351" TEXT="Just a local cache"/>
</node>
</node>
</node>
<node CREATED="1484393453688" ID="ID_1669246169" MODIFIED="1484393455020" TEXT="commands">
<node CREATED="1484378327867" ID="ID_420619115" MODIFIED="1484393602852" TEXT="internal commands (within agda directed at core)">
<node CREATED="1484378393804" ID="ID_634248273" MODIFIED="1484378395551" TEXT="connect">
<node CREATED="1484393466256" ID="ID_1130861553" MODIFIED="1484393468644" TEXT="connects to peer"/>
</node>
<node CREATED="1484378396060" ID="ID_421677146" MODIFIED="1484378397447" TEXT="send-msg">
<node CREATED="1484393471616" ID="ID_1171696930" MODIFIED="1484393474556" TEXT="sends a message"/>
</node>
<node CREATED="1484378397932" ID="ID_594978166" MODIFIED="1484378399607" TEXT="disconnect">
<node CREATED="1484393476032" ID="ID_1775938335" MODIFIED="1484393479964" TEXT="disconnects from peer"/>
</node>
<node CREATED="1484378597939" ID="ID_743478752" MODIFIED="1484379400292" TEXT="local cache">
<node CREATED="1484379400284" ID="ID_1083258717" MODIFIED="1484379401527" TEXT="commands">
<node CREATED="1484378400244" ID="ID_1935279384" MODIFIED="1484378538367" TEXT="update/insert/delete local cache">
<node CREATED="1484378412485" ID="ID_274847923" MODIFIED="1484378512761" TEXT="should we make any change only available when next message occurs?"/>
</node>
<node CREATED="1484378405475" ID="ID_648423827" MODIFIED="1484379437816" TEXT="note that local cache will be alterable code for the next message"/>
</node>
<node CREATED="1484379318333" ID="ID_1877202031" MODIFIED="1484392665732" TEXT="attached to block">
<node CREATED="1484392667096" ID="ID_629729703" MODIFIED="1484392676540" TEXT="goes forward to next block, but not backwards"/>
</node>
</node>
<node CREATED="1484378602324" ID="ID_497351232" MODIFIED="1484379404347" TEXT="block change">
<node CREATED="1484379404340" ID="ID_327243039" MODIFIED="1484379405456" TEXT="commands">
<node CREATED="1484378547276" ID="ID_1640536819" MODIFIED="1484378572441" TEXT="update/insert/delete block"/>
<node CREATED="1484378612548" ID="ID_848385769" MODIFIED="1484392690924" TEXT="creates a new tree"/>
</node>
</node>
<node CREATED="1484393324088" ID="ID_1968603380" MODIFIED="1484393327796" TEXT="create evidence">
<node CREATED="1484393329344" ID="ID_47150779" MODIFIED="1484393375124" TEXT="this creates a partial block which contains all key value pairs to support a chosen message"/>
</node>
<node CREATED="1484392710008" ID="ID_1863528137" MODIFIED="1484392714996" TEXT="block load">
<node CREATED="1484392715944" ID="ID_1613261007" MODIFIED="1484392756284" TEXT="partial block can be used to fill out tree">
<node CREATED="1484392757328" ID="ID_1680436920" MODIFIED="1484392911190" TEXT="usually encoded within a message">
<node CREATED="1484392911184" ID="ID_1936665352" MODIFIED="1484392912524" TEXT="example">
<node CREATED="1484392765824" ID="ID_442365321" MODIFIED="1484392918548" TEXT="transfer from account 1 to account 2 with proof of funds in account 1 by this tree"/>
</node>
</node>
</node>
<node CREATED="1484392932530" ID="ID_616194614" MODIFIED="1484392953164" TEXT="what if evidence is old, ie part of an old block from a long time ago.">
<node CREATED="1484392973920" ID="ID_73959079" MODIFIED="1484392993548" TEXT="it would have to be built up to the current block chain to check for double spending"/>
</node>
<node CREATED="1484393191065" ID="ID_812856858" MODIFIED="1484393200196" TEXT="what if evidence is from an abandoned block?">
<node CREATED="1484393202456" ID="ID_1906221487" MODIFIED="1484393286524" TEXT="evidence must be retrieved again"/>
<node CREATED="1484393286880" ID="ID_1064123776" MODIFIED="1484393305621" TEXT="supporting reversal doesn&apos;t seem all that useful"/>
</node>
</node>
</node>
<node CREATED="1484393616704" ID="ID_129461329" MODIFIED="1484393630891" TEXT="external commands (user to outer core)"/>
<node CREATED="1484393558712" ID="ID_548811119" MODIFIED="1484393564988" TEXT="what about evidence verification?">
<node CREATED="1484393566000" ID="ID_664707794" MODIFIED="1484393586372" TEXT="suppose evidence is from an old block for example"/>
</node>
</node>
</node>
<node CREATED="1484546842353" FOLDED="true" ID="ID_990121439" MODIFIED="1490410965015" TEXT="1/16/17">
<node CREATED="1484546847536" ID="ID_666431249" MODIFIED="1484546853029" TEXT="commands">
<node CREATED="1484546854337" ID="ID_32064849" MODIFIED="1484547506691" TEXT="user to outer core">
<node CREATED="1484546870713" ID="ID_1651925758" MODIFIED="1484546892341" TEXT="get-first-block">
<node CREATED="1484547526833" ID="ID_318349405" MODIFIED="1484547533933" TEXT="(context name)"/>
</node>
<node CREATED="1484546892721" ID="ID_289557262" MODIFIED="1484546896925" TEXT="get-next-blocks">
<node CREATED="1484547526833" ID="ID_485858574" MODIFIED="1484547543669" TEXT="(context name, block ref)"/>
<node CREATED="1484547514321" ID="ID_786705412" MODIFIED="1484547523149" TEXT="returns any immediate children of a block"/>
</node>
<node CREATED="1484547506681" ID="ID_1710869851" MODIFIED="1484547507837" TEXT="???">
<node CREATED="1484546991226" ID="ID_989862737" MODIFIED="1484547057085" TEXT="get-ref (block hash, key)"/>
<node CREATED="1484547063536" ID="ID_1312730260" MODIFIED="1484547068269" TEXT="get-value (block hash, key)"/>
<node CREATED="1484546921042" ID="ID_1968207320" MODIFIED="1484548579205" TEXT="query-state">
<node CREATED="1484546941329" ID="ID_1492700065" MODIFIED="1484546964509" TEXT="(block hash, method-name, args)">
<node CREATED="1484547076105" ID="ID_1157987000" MODIFIED="1484547095565" TEXT="args are references?">
<node CREATED="1484547098914" ID="ID_554954046" MODIFIED="1484547109557" TEXT="should we allow vars as well?"/>
</node>
</node>
<node CREATED="1484548525722" ID="ID_1268705067" MODIFIED="1484548543221" TEXT="This should work just like Ctrl-C, Ctrl-N works in emacs in agda-mode"/>
</node>
</node>
</node>
<node CREATED="1484548782265" ID="ID_1796053655" MODIFIED="1484548794477" TEXT="user to inner core (via outer core)">
<node CREATED="1484548702649" ID="ID_1214649224" MODIFIED="1484548872886" TEXT="receive-local-msg">
<node CREATED="1484548749561" ID="ID_516345985" MODIFIED="1484548763597" TEXT="sends a message to the inner core, designate by the user"/>
</node>
</node>
<node CREATED="1484548813753" ID="ID_142949705" MODIFIED="1484548821005" TEXT="inner core to user (via outer core)">
<node CREATED="1484548810849" ID="ID_218933505" MODIFIED="1484548876093" TEXT="send-local-msg">
<node CREATED="1484548881961" ID="ID_1454912755" MODIFIED="1484548889749" TEXT="send a message to the user"/>
</node>
</node>
<node CREATED="1484546908193" ID="ID_1113626056" MODIFIED="1484546911685" TEXT="outer core to user">
<node CREATED="1484546912978" ID="ID_1881950530" MODIFIED="1484546918549" TEXT="notify-new-block"/>
</node>
<node CREATED="1484546861321" ID="ID_1477325263" MODIFIED="1484546865189" TEXT="outer core to inner core">
<node CREATED="1484547807105" ID="ID_1080549753" MODIFIED="1484547883661" TEXT="process-msg"/>
<node CREATED="1484547884121" ID="ID_383093730" MODIFIED="1484547892677" TEXT="create-fill-in-tree-request"/>
</node>
<node CREATED="1484546865441" ID="ID_1616959761" MODIFIED="1484546868637" TEXT="inner core to outer core">
<node CREATED="1484547897745" ID="ID_1691539618" MODIFIED="1484547904118" TEXT="store-block">
<node CREATED="1484547904977" ID="ID_986627601" MODIFIED="1484547928973" TEXT="(parent-block, change-tree, block-root)">
<node CREATED="1484547919497" ID="ID_223917366" MODIFIED="1484547922373" TEXT="parent is optional"/>
</node>
</node>
<node CREATED="1484547933498" ID="ID_102940454" MODIFIED="1484547945061" TEXT="get-child-blocks">
<node CREATED="1484547946017" ID="ID_1378256229" MODIFIED="1484547948589" TEXT="(parent-block)"/>
</node>
<node CREATED="1484547949929" ID="ID_1006487937" MODIFIED="1484547960974" TEXT="update-local-cache">
<node CREATED="1484547962073" ID="ID_1757408164" MODIFIED="1484547967973" TEXT="(block, change-tree)"/>
<node CREATED="1484547972377" ID="ID_1785832575" MODIFIED="1484548004869" TEXT="local cache is kept with block. Child blocks initially inherit parent blocks local cache"/>
</node>
<node CREATED="1484548028377" ID="ID_872957905" MODIFIED="1484548032789" TEXT="delete-block">
<node CREATED="1484548038897" ID="ID_89414843" MODIFIED="1484548049373" TEXT="(block)"/>
<node CREATED="1484548050345" ID="ID_1985239167" MODIFIED="1484548090077" TEXT="deletes a block from storage (for triming abandoned branches)"/>
</node>
<node CREATED="1484548090993" ID="ID_695751392" MODIFIED="1484548099797" TEXT="connect/disconnect peer"/>
<node CREATED="1484548100041" ID="ID_1895141832" MODIFIED="1484548103077" TEXT="send-msg to peer"/>
<node CREATED="1484548103561" ID="ID_226794146" MODIFIED="1484548107397" TEXT="fill-in-tree">
<node CREATED="1484548109530" ID="ID_848672175" MODIFIED="1484548112701" TEXT="(change-tree, block)"/>
<node CREATED="1484548113009" ID="ID_1864474616" MODIFIED="1484548137046" TEXT="Fills the tree in with values for various keys. Keys must comply with block hash"/>
</node>
</node>
</node>
<node CREATED="1484564883691" ID="ID_1520174863" MODIFIED="1484564887937" TEXT="why no lambda auth">
<node CREATED="1484564888964" ID="ID_47011465" MODIFIED="1484564963983">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      lambda auth is used for&#160;data. I want data and code together within the new system. In this way, you don't need to retrieve, decode, process, and then encode, but rather treat data like any other structure within agda.
    </p>
  </body>
</html></richcontent>
<font NAME="SansSerif" SIZE="12"/>
</node>
</node>
<node CREATED="1484565009444" ID="ID_1456279341" MODIFIED="1484565029648" TEXT="I want the client to use agda directly. Ie. user interaction is *in* agda">
<node CREATED="1484549313257" ID="ID_1147995506" MODIFIED="1484549329525" TEXT="maybe we should extend beyond strings for messages between peers">
<node CREATED="1484549330841" ID="ID_238365279" MODIFIED="1484549366653" TEXT="if peers communicated using the trees themselves, it may be easier for the contexts to process the data"/>
</node>
</node>
</node>
<node CREATED="1484817143382" FOLDED="true" ID="ID_1389792202" MODIFIED="1490410965944" TEXT="1/19/17">
<node CREATED="1484817148812" ID="ID_1669871982" MODIFIED="1484817336995" TEXT="basic idea now is the tree is altered">
<node CREATED="1484817336957" ID="ID_414237209" MODIFIED="1484817338296" TEXT="commands">
<node CREATED="1484817339661" ID="ID_454929853" MODIFIED="1484817477320" TEXT="move">
<node CREATED="1484817345692" ID="ID_216653653" MODIFIED="1484817492681" TEXT="renames a key">
<node CREATED="1484817494868" ID="ID_852979914" MODIFIED="1484817513009" TEXT="can be construed as delete if a key is renamed to null"/>
</node>
</node>
<node CREATED="1484870866483" ID="ID_1337419560" MODIFIED="1484870875974" TEXT="(only one command related to tree altering)"/>
</node>
<node CREATED="1484817380212" ID="ID_1883784159" MODIFIED="1484817388609" TEXT="the control of these commands is up to the module"/>
</node>
<node CREATED="1484817209196" ID="ID_297661631" MODIFIED="1484817237722" TEXT="a new block can have any data inside of it with the following restrictions">
<node CREATED="1484817239252" ID="ID_1041527394" MODIFIED="1484817645152" TEXT="Existing modules cannot be altered (unless using the move command)"/>
</node>
<node CREATED="1484870742699" ID="ID_1288283038" MODIFIED="1484914274831" TEXT="storage">
<node CREATED="1484914274798" ID="ID_323011597" MODIFIED="1484914277273" TEXT="thoughts">
<node CREATED="1484913820287" ID="ID_859326264" MODIFIED="1484913850841" TEXT="bitcoin has a pruning mechanism in the original white paper where transactions are pruned from old blocks involving spent transactions"/>
<node CREATED="1484913854405" ID="ID_1951123816" MODIFIED="1484913874776" TEXT="this would be akin to miners forgetting old blocks in the new system"/>
<node CREATED="1484913875324" ID="ID_133565689" MODIFIED="1484913900288" TEXT="the only difference here is that without all the blocks, we can not know what the current state of the tree is"/>
<node CREATED="1484913905068" ID="ID_1444338591" MODIFIED="1484914039152" TEXT="we already make every node store the root hash, so this would not be a problem"/>
<node CREATED="1484914039588" ID="ID_1746793319" MODIFIED="1484914072865" TEXT="what is a problem, however, is that there is no rule against referencing a key that comes from a very old block that miners would likely not store"/>
</node>
<node CREATED="1484870929874" ID="ID_1265255034" MODIFIED="1485001256619" TEXT="how to prove storage of a block?">
<font BOLD="true" NAME="SansSerif" SIZE="12"/>
<node CREATED="1484914115164" ID="ID_1636688816" MODIFIED="1484914246504" TEXT="we force miners to lookup a random key, and take a difference hash of it">
<node CREATED="1484914143493" ID="ID_1884464619" MODIFIED="1484914228626" TEXT="then all other miners would also need that same value, which is ok, they should have it"/>
<node CREATED="1484914153741" ID="ID_1074903905" MODIFIED="1484914162441" TEXT="we could limit the blocks that the miners have to look at"/>
<node CREATED="1484914162972" ID="ID_105625963" MODIFIED="1484914192616" TEXT="we can even have a mechanism for removing keys that are not necessary anymore"/>
</node>
</node>
</node>
<node CREATED="1484870734178" ID="ID_532978513" MODIFIED="1484870736582" TEXT="mining">
<node CREATED="1484870759114" ID="ID_847374936" MODIFIED="1484870825094" TEXT="mining will hash against the following fields">
<node CREATED="1484870794570" ID="ID_1389166490" MODIFIED="1484870801926" TEXT="hash of previous block"/>
<node CREATED="1484870802418" ID="ID_824515504" MODIFIED="1484870803639" TEXT="nonce"/>
<node CREATED="1484870804010" ID="ID_197086084" MODIFIED="1484870811750" TEXT="miner address"/>
</node>
</node>
<node CREATED="1484915471070" ID="ID_1555370011" MODIFIED="1484915473888" TEXT="cache">
<node CREATED="1484915475269" ID="ID_1036014752" MODIFIED="1485000922318" TEXT="will be done using FFI">
<node CREATED="1485000923881" ID="ID_1996221874" MODIFIED="1485000930422" TEXT="specifically, the &quot;postulate&quot; command"/>
<node CREATED="1485000930953" ID="ID_1553599739" MODIFIED="1485000937765" TEXT="must be in a special set of packages"/>
<node CREATED="1485000940082" ID="ID_1711907698" MODIFIED="1485000996653" TEXT="Each cache item must have a corresponding &quot;default&quot; key/value"/>
<node CREATED="1485001029898" ID="ID_32764775" MODIFIED="1485001068366" TEXT="how to prevent using cache to attack system by creating &quot;postulates&quot; of invalid proofs?">
<node CREATED="1485001103329" ID="ID_1993386200" MODIFIED="1485001110926" TEXT="The default key/value that is required"/>
</node>
</node>
</node>
</node>
<node CREATED="1485157399176" FOLDED="true" ID="ID_155265443" MODIFIED="1510030853783" TEXT="1/23/17">
<node CREATED="1485157403740" ID="ID_1112097933" MODIFIED="1485157404888" TEXT="blog post">
<node CREATED="1485157405789" ID="ID_472216292" MODIFIED="1485157412504" TEXT="Overview"/>
<node CREATED="1485157412900" ID="ID_152452073" MODIFIED="1485157437936" TEXT="Block Creation"/>
<node CREATED="1485170442094" ID="ID_1961463343" MODIFIED="1485170447930" TEXT="Types of data">
<node CREATED="1485170449262" ID="ID_1394509112" MODIFIED="1485170450490" TEXT="Cache">
<node CREATED="1485170620822" ID="ID_824807768" MODIFIED="1485170625866" TEXT="Network"/>
<node CREATED="1485170626791" ID="ID_1498888807" MODIFIED="1485170629281" TEXT="UI">
<node CREATED="1485170634062" ID="ID_629562050" MODIFIED="1485170643673" TEXT="(FUTURE)"/>
</node>
</node>
<node CREATED="1485170451167" ID="ID_616662969" MODIFIED="1485170458027" TEXT="Options">
<node CREATED="1485170484894" ID="ID_300408592" MODIFIED="1485170493826" TEXT="user specified options (with defaults)"/>
</node>
<node CREATED="1485170479886" ID="ID_1676617269" MODIFIED="1485312231104" TEXT="State">
<node CREATED="1485312231067" ID="ID_1432907458" MODIFIED="1485312233403" TEXT="system">
<node CREATED="1485170496614" ID="ID_118582765" MODIFIED="1485170515307" TEXT="child blocks"/>
<node CREATED="1485170515870" ID="ID_192074995" MODIFIED="1485170531827" TEXT="outer core version"/>
<node CREATED="1485170520582" ID="ID_155533297" MODIFIED="1485170533657" TEXT="os"/>
<node CREATED="1485170535319" ID="ID_404249103" MODIFIED="1485170536009" TEXT="ip"/>
</node>
<node CREATED="1485312233734" ID="ID_1029772247" MODIFIED="1485312316986" TEXT="user?">
<node CREATED="1485312236550" ID="ID_635943610" MODIFIED="1485312241690" TEXT="private keys"/>
<node CREATED="1485312242006" ID="ID_167832850" MODIFIED="1485312280943" TEXT="social media">
<node CREATED="1485312280934" ID="ID_305906784" MODIFIED="1485312282154" TEXT="blobs">
<node CREATED="1485312262711" ID="ID_1566077124" MODIFIED="1485312263986" TEXT="pictures"/>
<node CREATED="1485312300078" ID="ID_1468652822" MODIFIED="1485312301106" TEXT="video"/>
<node CREATED="1485312302062" ID="ID_321180511" MODIFIED="1485312308186" TEXT="audio"/>
</node>
<node CREATED="1485312264278" ID="ID_543844557" MODIFIED="1485312284810" TEXT="text"/>
</node>
<node CREATED="1485312287990" ID="ID_1757911247" MODIFIED="1485312298026" TEXT="local rules?"/>
</node>
</node>
<node CREATED="1485170676942" ID="ID_709097640" MODIFIED="1485170678466" TEXT="Core">
<node CREATED="1485170683062" ID="ID_1751782832" MODIFIED="1485170688891" TEXT="Merkle tree of data"/>
<node CREATED="1485170691742" ID="ID_294025061" MODIFIED="1485170693873" TEXT="sub contexts">
<node CREATED="1485170694878" ID="ID_877516255" MODIFIED="1485170699259" TEXT="merkle trees of data"/>
<node CREATED="1485170699518" ID="ID_184600234" MODIFIED="1485170702963" TEXT="overlays parent"/>
</node>
</node>
<node CREATED="1485170679862" ID="ID_632311166" MODIFIED="1485170680873" TEXT="Blocks">
<node CREATED="1485170710646" ID="ID_796327366" MODIFIED="1485170717235" TEXT="merkle tree of data"/>
<node CREATED="1485170718110" ID="ID_1398181415" MODIFIED="1485170732626" TEXT="== scratch module"/>
</node>
</node>
<node CREATED="1485157440805" ID="ID_1256468241" MODIFIED="1485170819566" TEXT="Sub Contexts">
<node CREATED="1485157446156" ID="ID_423467292" MODIFIED="1485157452391" TEXT="Merkle Trees of data"/>
<node CREATED="1485157452805" ID="ID_204118110" MODIFIED="1485157469783" TEXT="Each sub context is an overlay of its parents"/>
<node CREATED="1485157470084" ID="ID_1798581532" MODIFIED="1485157475279" TEXT="all communication through root"/>
</node>
<node CREATED="1485157438388" ID="ID_1855981300" MODIFIED="1485170771947" TEXT="block hierarchy">
<node CREATED="1485158853757" ID="ID_289107509" MODIFIED="1485158908256" TEXT="Controlling block">
<node CREATED="1485170549895" ID="ID_681672633" MODIFIED="1485170558297" TEXT="Each block computes a score">
<node CREATED="1485170577942" ID="ID_1844848189" MODIFIED="1485170583002" TEXT="Maybe depth as well???"/>
</node>
<node CREATED="1485170558734" ID="ID_791889374" MODIFIED="1485170589938" TEXT="Controlling block gets score / depth of its children">
<node CREATED="1485170591510" ID="ID_1788785288" MODIFIED="1485170604123" TEXT="relinquishes control when child has high enough depth"/>
<node CREATED="1485170605470" ID="ID_1535101370" MODIFIED="1485170615610" TEXT="(this may also depend on user options)"/>
</node>
</node>
<node CREATED="1485170779006" ID="ID_626902340" MODIFIED="1485170782930" TEXT="sub contexts">
<node CREATED="1485170783799" ID="ID_1113914668" MODIFIED="1485170787298" TEXT="add new block in sub context"/>
<node CREATED="1485170803462" ID="ID_451593607" MODIFIED="1485170808050" TEXT="controlling block in sub context"/>
</node>
<node CREATED="1485170791334" ID="ID_1700867449" MODIFIED="1485170792130" TEXT="reorg">
<node CREATED="1485170793103" ID="ID_701539960" MODIFIED="1485170794082" TEXT="root"/>
<node CREATED="1485170794366" ID="ID_1696951905" MODIFIED="1485170796489" TEXT="sub contexts"/>
</node>
</node>
</node>
</node>
<node CREATED="1489390477991" FOLDED="true" ID="ID_1087713835" MODIFIED="1490698236654" TEXT="3/13/17">
<node CREATED="1489390489143" ID="ID_723387743" MODIFIED="1489390503803" TEXT="knowledgebase">
<node CREATED="1489390505527" ID="ID_865119745" MODIFIED="1489390516539" TEXT="I&apos;m thinking of not using a knowledge base">
<node CREATED="1489390520959" ID="ID_780425934" MODIFIED="1489390558267" TEXT="I want most facts directly loaded within Agda, so that proofs can be created using values of known constants">
<node CREATED="1489390564095" ID="ID_1482722110" MODIFIED="1489390606037">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      Ex.&#160;
    </p>
    <p>
      Foo : Int
    </p>
    <p>
      foo = 42
    </p>
    <p>
      
    </p>
    <p>
      Bar : Int
    </p>
    <p>
      bar = 13 + foo
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1489390607247" ID="ID_955192094" MODIFIED="1489390624267" TEXT="With the above, bar can be proved to equal 55"/>
<node CREATED="1489390627263" ID="ID_908914280" MODIFIED="1489390645899" TEXT="Using some external kb, we can&apos;t know ahead of time what the value is"/>
</node>
</node>
</node>
<node CREATED="1489390761471" ID="ID_394905470" MODIFIED="1489390764883" TEXT="doc">
<node CREATED="1489390765775" ID="ID_688135481" MODIFIED="1489390773987" TEXT="Open Phase management">
<node CREATED="1489390820983" ID="ID_534567126" MODIFIED="1489390824011" TEXT="insight">
<node CREATED="1489390824887" ID="ID_393717291" MODIFIED="1489390836579" TEXT="parallel to bitcoin app, etc.">
<node CREATED="1489390837639" ID="ID_812606837" MODIFIED="1489390852995" TEXT="each open phase program can be considered the same as the client for any other coin"/>
<node CREATED="1489390853207" ID="ID_1959111953" MODIFIED="1489390863803" TEXT="it has certain purposes and runs certain commands"/>
</node>
</node>
<node CREATED="1489390807367" ID="ID_973589096" MODIFIED="1489390820491" TEXT="closed phase creation"/>
</node>
<node CREATED="1489390776263" ID="ID_1010069073" MODIFIED="1489390791107" TEXT="Closed Phase operations">
<node CREATED="1489390791887" ID="ID_1315281300" MODIFIED="1489390792811" TEXT="action"/>
<node CREATED="1489390794895" ID="ID_256514927" MODIFIED="1489390795699" TEXT="query"/>
</node>
<node CREATED="1489391796615" ID="ID_1508094995" MODIFIED="1489391813227" TEXT="Example flow">
<node CREATED="1489392326519" ID="ID_150722775" MODIFIED="1489392331715" TEXT="bootstrap">
<node CREATED="1489392334663" ID="ID_1602946750" MODIFIED="1489392370243" TEXT="Bootstrap will be a static &quot;genesis&quot; closed phase program"/>
<node CREATED="1489392372855" ID="ID_1488800876" MODIFIED="1489392394195" TEXT="Genesis will create an Open Phase server to generate a network and start processing blocks from peers"/>
</node>
<node CREATED="1489392415231" ID="ID_626531357" MODIFIED="1489392428667" TEXT="Root context blocks received">
<node CREATED="1489392477975" ID="ID_596914141" MODIFIED="1489392627634" TEXT="The already existing open phase server will receive a block from peers and contact the Kernel, notifying it of the block and its parent"/>
<node CREATED="1489392628734" ID="ID_321364482" MODIFIED="1489392679819" TEXT="If the block is not currently represented in the current closed phase snapshot, a new closed phase snapshot of the parent block will be created"/>
<node CREATED="1489392681063" ID="ID_1681763179" MODIFIED="1489392721043" TEXT="The kernel will notify the CP snapshot of the block, which will proceed to accept or reject it, providing a score if accepted"/>
<node CREATED="1489392696519" ID="ID_841112029" MODIFIED="1489392777331" TEXT="If accepted, the kernel will add the block to the block chain with the given score"/>
<node CREATED="1489392777823" ID="ID_658149191" MODIFIED="1489392796563" TEXT="If the new block has a higher score than the current CP snapshot, the current CP snapshot will be updated"/>
<node CREATED="1489392797111" ID="ID_495571849" MODIFIED="1489392824571" TEXT="The current CP will receive a &quot;reorg&quot; message from the kernel, which gives it a chance to update the open phase code if necessary"/>
</node>
<node CREATED="1489392833087" ID="ID_1567122539" MODIFIED="1489392837971" TEXT="Sub context blocks received">
<node CREATED="1489392839543" ID="ID_419863111" MODIFIED="1489392853883" TEXT="Proceeds exactly as root context"/>
</node>
<node CREATED="1489391814519" ID="ID_445472558" MODIFIED="1489392897578" TEXT="User subscribes to existing Sub context">
<node CREATED="1489391828223" ID="ID_1327794484" MODIFIED="1489391931427" TEXT="User creates Scratch module to run closed phase action">
<node CREATED="1489391935599" ID="ID_1602041887" MODIFIED="1489391939547" TEXT="Subscribe sub context"/>
</node>
<node CREATED="1489391950415" ID="ID_603737834" MODIFIED="1489392192308" TEXT="Closed Phase notifies server (in open phase) to ask for root of sub context"/>
<node CREATED="1489392203791" ID="ID_1848821931" MODIFIED="1489392242315" TEXT="Server downloads sub context from appropriate peers, and runs closed phase command to create new sub context"/>
<node CREATED="1489392243447" ID="ID_1801685476" MODIFIED="1489392277763" TEXT="Closed Phase creates new Open Phase program(s) to manage sub context">
<node CREATED="1489392441495" ID="ID_1598000060" MODIFIED="1489392443955" TEXT="Insight">
<node CREATED="1489392444951" ID="ID_1332805052" MODIFIED="1489392470483" TEXT="Only users subscribed to a subcontext will have to bother reading the block chain, and the nature of this is up to the sub context"/>
</node>
</node>
<node CREATED="1489392278295" ID="ID_1297156634" MODIFIED="1489392319779" TEXT="new Open Phase program(s) download new sub context blocks to update sub context to sync block chain"/>
</node>
<node CREATED="1489392898191" ID="ID_1543238827" MODIFIED="1489392903275" TEXT="User creates a new sub context">
<node CREATED="1489392905126" ID="ID_1999161273" MODIFIED="1489392938571" TEXT="User creates a Scratch module with sub-context closed phase code, and calls a function in the root context"/>
<node CREATED="1489393063503" ID="ID_1326979838" MODIFIED="1489393092963" TEXT="Root context will publish it in the next block"/>
<node CREATED="1489393130639" ID="ID_1140464139" MODIFIED="1489393132739" TEXT="Insight">
<node CREATED="1489393133479" ID="ID_574108427" MODIFIED="1489393139931" TEXT="All contexts are created equal"/>
<node CREATED="1489393140591" ID="ID_1197559277" MODIFIED="1489393162403" TEXT="The only difference is that the bootstrap genesis subscribes the user to the Root context initially."/>
<node CREATED="1489393162903" ID="ID_110013460" MODIFIED="1489393199819" TEXT="Because users are subscribed to the root context, they will be able to subscribe to any contexts that are published to it"/>
</node>
</node>
</node>
<node CREATED="1489391679343" ID="ID_1093029241" MODIFIED="1489391682771" TEXT="Operations">
<node CREATED="1489391683951" ID="ID_1566092860" MODIFIED="1489391690835" TEXT="Kernel to Closed Phase">
<node CREATED="1489391691839" ID="ID_58639358" MODIFIED="1489391702571" TEXT="start"/>
<node CREATED="1489391704407" ID="ID_667972932" MODIFIED="1489391710563" TEXT="accept_child_block">
<node CREATED="1489391711575" ID="ID_864505" MODIFIED="1489391718075" TEXT="(change_list)"/>
<node CREATED="1489391718479" ID="ID_931102800" MODIFIED="1489391721347" TEXT="returns">
<node CREATED="1489391721879" ID="ID_1433981883" MODIFIED="1489391744239">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      accepted : bool
    </p>
    <p>
      child_score : int
    </p>
  </body>
</html></richcontent>
</node>
</node>
</node>
</node>
</node>
<node CREATED="1489390799759" ID="ID_412658723" MODIFIED="1489390802531" TEXT="Cache"/>
<node CREATED="1489390870615" ID="ID_118677457" MODIFIED="1489390873491" TEXT="Reflection?">
<node CREATED="1489390876335" ID="ID_1402394069" MODIFIED="1489390882259" TEXT="Alternative to external knowledge base"/>
<node CREATED="1489390889647" ID="ID_513934370" MODIFIED="1489390903595" TEXT="Not sure if necessary, why do we need to reason over ourself?"/>
</node>
</node>
</node>
<node CREATED="1490610565419" FOLDED="true" ID="ID_866379130" MODIFIED="1497252357693" TEXT="3/27/17">
<node CREATED="1490610696920" ID="ID_627905910" MODIFIED="1490610702635" TEXT="Plan">
<node CREATED="1490610705064" ID="ID_598086508" MODIFIED="1490610747556" TEXT="Create a simplified format mimicing the essential details of the Agda internal format or abstract format"/>
<node CREATED="1490610748007" ID="ID_1186246293" MODIFIED="1490610774836" TEXT="Create a database like structure for the huge amount of data necessary for a block chain using this format">
<node CREATED="1490610775864" ID="ID_208116988" MODIFIED="1490610793515" TEXT="Database should allow for quick verification of functions"/>
</node>
</node>
<node CREATED="1490610571456" ID="ID_820288031" MODIFIED="1490610574747" TEXT="Potential problems">
<node CREATED="1490610576584" ID="ID_1250010665" MODIFIED="1490610582963" TEXT="Unification">
<node CREATED="1490610583816" ID="ID_1885113355" MODIFIED="1490610588508" TEXT="For example">
<node CREATED="1490610588880" ID="ID_1306320374" MODIFIED="1490610613587">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      id : { A : Set } A -&gt; A
    </p>
    <p>
      id x = x
    </p>
  </body>
</html></richcontent>
<node CREATED="1490610616600" ID="ID_1780195404" MODIFIED="1490610651259" TEXT="If compiled as is, my program will need to fill in the &quot;hole&quot; left by {A : Set}"/>
<node CREATED="1490610654064" ID="ID_332194505" MODIFIED="1490610667563" TEXT="This seems difficult to get right but not impossible"/>
<node CREATED="1490610668184" ID="ID_1397617949" MODIFIED="1490610686235" TEXT="Agda documentation says it uses &quot;miller pattern unification&quot;"/>
</node>
</node>
</node>
</node>
</node>
<node CREATED="1494151602543" FOLDED="true" ID="ID_1762190615" MODIFIED="1494315026353" TEXT="5/7/17">
<node CREATED="1494151637226" ID="ID_210713936" MODIFIED="1494151657599" TEXT="problems and caveats">
<node CREATED="1494151649474" ID="ID_906130506" MODIFIED="1494151653798" TEXT="assumptions">
<node CREATED="1494151659026" ID="ID_55484934" MODIFIED="1494151674142" TEXT="when we deal with kernel operations, we are assuming they will behave properly">
<node CREATED="1494151675058" ID="ID_1388931387" MODIFIED="1494151741583" TEXT="we can&apos;t verify it, because (-&gt; == depends on) AN -&gt; os -&gt; hardware -&gt; laws of physics"/>
<node CREATED="1494151743091" ID="ID_185266172" MODIFIED="1494151769965" TEXT="so we have to assume the OS doesn&apos;t screw up our calls. It can&apos;t leak information and it can&apos;t fail in catastrophic ways"/>
<node CREATED="1494151771914" ID="ID_42942533" MODIFIED="1494151775814" TEXT="impossible to prove">
<node CREATED="1494151786082" ID="ID_503655843" MODIFIED="1494151816918" TEXT="however we can do checksums to minimize risk of catastropic failure (ex leaking of secret data due to hardware failure)"/>
</node>
</node>
</node>
<node CREATED="1494151830362" ID="ID_346513171" MODIFIED="1494151848390" TEXT="we can&apos;t let the prior block determine a score for a later block naively">
<node CREATED="1494151849402" ID="ID_417024320" MODIFIED="1494151883614" TEXT="Suppose we use PoW, and a black hat creats an errant block that changes all future block scores to be much bigger than before">
<node CREATED="1494151887098" ID="ID_593129888" MODIFIED="1494151895678" TEXT="naively the system would follow the black hat chain"/>
</node>
<node CREATED="1494151943465" ID="ID_684027685" MODIFIED="1494151982645" TEXT="genesis: The threshold for score changes must be at minimum an acceptable number of blocks which would make it infeasible for a black hat to change"/>
</node>
<node CREATED="1494152048530" ID="ID_770646417" MODIFIED="1494152052198" TEXT="we need a fucking coin">
<node CREATED="1494152053730" ID="ID_320813667" MODIFIED="1494152067406" TEXT="Without a coin we will be network affected out">
<node CREATED="1494152068490" ID="ID_1498867332" MODIFIED="1494152099196" TEXT="A coined blockchain will have a greater network effect due to marketing"/>
<node CREATED="1494152102593" ID="ID_1822054803" MODIFIED="1494152117245" TEXT="Any subcontext based on a coin will increase the coins value"/>
</node>
</node>
</node>
<node CREATED="1494212673906" ID="ID_1069019562" MODIFIED="1494212676269" TEXT="altruism">
<node CREATED="1494212677658" ID="ID_1056082720" MODIFIED="1494212689413" TEXT="altruism IRL is based on shaming">
<node CREATED="1494212690698" ID="ID_332066449" MODIFIED="1494212707358" TEXT="cut in a line people look down upon you and are less likely to help you"/>
<node CREATED="1494212707914" ID="ID_1383431032" MODIFIED="1494212713229" TEXT="you violated the customs"/>
<node CREATED="1494212715289" ID="ID_1467258107" MODIFIED="1494212730142" TEXT="in places like China, these customs don&apos;t exist, so the altruism doesn&apos;t exist either"/>
</node>
<node CREATED="1494212731810" ID="ID_954826534" MODIFIED="1494212891480" TEXT="on the internet, there is no shame">
<node CREATED="1494212891466" ID="ID_1519839321" MODIFIED="1494212908382" TEXT="information flow creates altruism">
<node CREATED="1494212742650" ID="ID_992334661" MODIFIED="1494213784142" TEXT="in bitcoin et al, it&apos;s an information problem, coupled with the reward being of little signficance">
<node CREATED="1494213735290" ID="ID_1131643155" MODIFIED="1494213765254" TEXT="The cost of sharing blocks and transaction information with other users is miniscle"/>
</node>
<node CREATED="1494212790738" ID="ID_1881486968" MODIFIED="1494212804038" TEXT="running a bitcoin node, you are expected to share with other nodes"/>
<node CREATED="1494212809338" ID="ID_1203815665" MODIFIED="1494213799447" TEXT="average users don&apos;t know/care there is an alternative">
<node CREATED="1494212852322" ID="ID_89252849" MODIFIED="1494212881598" TEXT="they don&apos;t want to spend the effort to learn the information, because the payout is so tiny"/>
</node>
<node CREATED="1494212916434" ID="ID_922156815" MODIFIED="1494212926110" TEXT="so they download the default, as decided by the developers"/>
<node CREATED="1494213285082" ID="ID_198697111" MODIFIED="1494213311166" TEXT="There is a limit to this, however. If it becomes too bothersome, someone will capitalize on the failure of information flow">
<node CREATED="1494213312849" ID="ID_205655600" MODIFIED="1494213346622" TEXT="Such as with ad blockers"/>
</node>
<node CREATED="1494213356810" ID="ID_1286214461" MODIFIED="1494213479630" TEXT="Once this failure occurs, it will shift to an extreme, but may balance out after a bit">
<node CREATED="1494213380754" ID="ID_646784755" MODIFIED="1494213423694" TEXT="Ad blockers used to block all ads, now some of them let some through">
<node CREATED="1494213433754" ID="ID_1772359764" MODIFIED="1494213459014" TEXT="(But part of the reasoning for that is the ad block detection built into websites)"/>
</node>
</node>
</node>
<node CREATED="1494212929026" ID="ID_1246670356" MODIFIED="1494212991222" TEXT="what about miners? They have a lot of knowledge and their margins are razor thin">
<node CREATED="1494212992650" ID="ID_570204854" MODIFIED="1494213014653" TEXT="Users want their transactions published to as many miners as possible"/>
<node CREATED="1494213015954" ID="ID_207770315" MODIFIED="1494213046142" TEXT="If miners aren&apos;t willing to share with each other, exchanges, blockchain explorer websites and enthuisists will be">
<node CREATED="1494213048354" ID="ID_1156115763" MODIFIED="1494213067542" TEXT="This is due to shaming, or rather the opposite of shaming. Recognition of altruism"/>
</node>
</node>
</node>
<node CREATED="1494213074458" ID="ID_235862849" MODIFIED="1494213082678" TEXT="how does this apply to autonomic">
<node CREATED="1494213083826" ID="ID_554372192" MODIFIED="1494213090430" TEXT="Block sharing">
<node CREATED="1494213092114" ID="ID_630255452" MODIFIED="1494213103142" TEXT="Information flow?">
<node CREATED="1494213104562" ID="ID_1259398799" MODIFIED="1494213152198" TEXT="The average user has to download some sort of bootstrap, which will have rules in it pertaining to block sharing"/>
<node CREATED="1494213194842" ID="ID_1815049582" MODIFIED="1494213246702" TEXT="The rules of the block chain will have &quot;recommendations&quot; pertaining to block sharing"/>
<node CREATED="1494213260834" ID="ID_775800618" MODIFIED="1494213278838" TEXT="The average user will follow the recommendations, until they get too bothersome that is">
<node CREATED="1494213495786" ID="ID_253044910" MODIFIED="1494213528910" TEXT="If the user has to download too much data, or bandwidth becomes too high, it will shift to an extreme to the other way"/>
</node>
</node>
</node>
</node>
</node>
<node CREATED="1494214293394" ID="ID_1177878918" MODIFIED="1494214296118" TEXT="genesis">
<node CREATED="1494214306762" ID="ID_85622770" MODIFIED="1494214332790" TEXT="idea A">
<node CREATED="1494214333818" ID="ID_193965387" MODIFIED="1494214375582" TEXT="rule changes are proposed at block x"/>
<node CREATED="1494214347106" ID="ID_322913307" MODIFIED="1494214373182" TEXT="rule changes go through at block x + K where K is a constant"/>
<node CREATED="1494214378851" ID="ID_1730891475" MODIFIED="1494214447319" TEXT="miners can signal support for a proposal"/>
</node>
<node CREATED="1494214448266" ID="ID_454638840" MODIFIED="1494214529977" TEXT="idea B">
<node CREATED="1494214529963" ID="ID_1813222192" MODIFIED="1494214531886" TEXT="plan">
<node CREATED="1494214450674" ID="ID_1541206525" MODIFIED="1494214469183" TEXT="any miner can make any change to rules at any time"/>
<node CREATED="1494214471770" ID="ID_1586296738" MODIFIED="1494214496086" TEXT="this creates a fork, miners that want to support the change follow the fork"/>
</node>
<node CREATED="1494214525065" ID="ID_1841667236" MODIFIED="1494214537414" TEXT="analysis">
<node CREATED="1494214538514" ID="ID_1480914689" MODIFIED="1494214553318" TEXT="problem is that could easily split into multiple long living forks">
<node CREATED="1494214558490" ID="ID_1613990174" MODIFIED="1494214562470" TEXT="ex ETC vs ETH"/>
</node>
</node>
</node>
<node CREATED="1494214582186" ID="ID_1432845030" MODIFIED="1494214584117" TEXT="idea C">
<node CREATED="1494214585179" ID="ID_1275895450" MODIFIED="1494214588030" TEXT="plan">
<node CREATED="1494214589066" ID="ID_233609288" MODIFIED="1494214597374" TEXT="constant genesis"/>
<node CREATED="1494214600427" ID="ID_716521431" MODIFIED="1494214619438" TEXT="only sub contexts may make changes"/>
</node>
<node CREATED="1494214627282" ID="ID_41334826" MODIFIED="1494214630846" TEXT="analysis">
<node CREATED="1494214632194" ID="ID_424444380" MODIFIED="1494214678526" TEXT="pretty much the same as idea B, but forks would have to be done in a hard manner. There is no inbuilt support for a fork">
<node CREATED="1494214699130" ID="ID_1851247079" MODIFIED="1494214716917" TEXT="should help prevent multiple long living forks, since it feeds into the information problem">
<node CREATED="1494214718018" ID="ID_1133809392" MODIFIED="1494214731998" TEXT="people need to understand the new software to install it, and gaining that knowledge requires work"/>
</node>
</node>
</node>
</node>
</node>
</node>
<node CREATED="1494311303878" FOLDED="true" ID="ID_1949947182" MODIFIED="1495697516186" TEXT="5/9/17">
<node CREATED="1494311319469" ID="ID_1704634606" MODIFIED="1494311321257" TEXT="genesis">
<node CREATED="1494311322332" ID="ID_730147776" MODIFIED="1494311332185" TEXT="we use idea A"/>
<node CREATED="1494311339373" ID="ID_92131707" MODIFIED="1494311351322" TEXT="a merkle structure root is stored in block"/>
<node CREATED="1494311352166" ID="ID_1892133166" MODIFIED="1494311375750" TEXT="has the following rules">
<node CREATED="1494311375740" ID="ID_783272376" MODIFIED="1494315043253" TEXT="mutable">
<node CREATED="1494311363285" ID="ID_1571081742" MODIFIED="1494311382289" TEXT="ability to change mutable rules using voting">
<node CREATED="1494311523941" ID="ID_873465765" MODIFIED="1494311557256" TEXT="voting is meant to emulate what would occur if a fork happened, without the destructive nature of a fork"/>
<node CREATED="1494311557716" ID="ID_65465450" MODIFIED="1494311627177" TEXT="51% of the mining power for a particular proposal over a long enough period of time (ex. a couple of days) to reduce the chances of a &lt;50% attacker causing a rule change"/>
</node>
<node CREATED="1494315043239" ID="ID_317427498" MODIFIED="1494315045354" TEXT="interfaces">
<node CREATED="1494312090797" ID="ID_500598902" MODIFIED="1494312100272" TEXT="provides an interface for">
<node CREATED="1494312103604" ID="ID_922492989" MODIFIED="1494312129513" TEXT="starting up a P2P network on behalf of a subcontext"/>
<node CREATED="1494312136629" ID="ID_885342360" MODIFIED="1494312152745" TEXT="adding data to the merkle structure"/>
<node CREATED="1494312167629" ID="ID_1616099957" MODIFIED="1494312168633" TEXT="????"/>
</node>
<node CREATED="1494315046038" ID="ID_124701899" MODIFIED="1494315072961" TEXT="all interfaces may have a proof requirement associated with them, for various restrictions that users want"/>
</node>
</node>
<node CREATED="1494311383164" ID="ID_1794835680" MODIFIED="1494311385016" TEXT="immutable">
<node CREATED="1494311385980" ID="ID_1018459685" MODIFIED="1494311399753" TEXT="query network for">
<node CREATED="1494311400733" ID="ID_305260893" MODIFIED="1494311421808" TEXT="a merkle hash without content to receive content"/>
</node>
<node CREATED="1494312006477" ID="ID_657506092" MODIFIED="1494312011177" TEXT="??? others ???">
<node CREATED="1494312012453" ID="ID_479876007" MODIFIED="1494312052401" TEXT="We don&apos;t want a &quot;from genesis&quot; user to be able to query, but not get any response due to the root data missing"/>
</node>
</node>
<node CREATED="1494311666629" ID="ID_1634132058" MODIFIED="1494311748352" TEXT="The immutable rules are meant to make sure that as long as there is no hard fork, a client may build up from the genesis. In other words, the network can&apos;t change to a point where a client working from genesis is no longer able to communicate with it">
<node CREATED="1494311486093" ID="ID_797216176" MODIFIED="1494312003488" TEXT="note that immutable rules can of course be forked (like all block chains), which provides an ability for autonomic to evolve, if need be. By making a hard fork necessary, (which we do on purpose), we make all users have to download a new client, rather than just &quot;build from genesis&quot; users. This keeps the network from becoming splintered, where some &quot;from geneisis&quot; users may face different problems/bugs then up to date users.">
<font NAME="SansSerif" SIZE="12"/>
</node>
</node>
</node>
<node CREATED="1494312063717" ID="ID_1850101809" MODIFIED="1494312071608" TEXT="creates the root chain">
<node CREATED="1494312072581" ID="ID_236421986" MODIFIED="1494312081569" TEXT="which is the only chain that may contact the outer core directly"/>
</node>
</node>
<node CREATED="1494332371721" ID="ID_936547009" MODIFIED="1494332373412" TEXT="root block">
<node CREATED="1494332376257" ID="ID_46345070" MODIFIED="1494332385973" TEXT="proposition">
<node CREATED="1494332386858" ID="ID_1152448739" MODIFIED="1494332390132" TEXT="(code change)"/>
</node>
<node CREATED="1494332391024" ID="ID_935286687" MODIFIED="1494332399380" TEXT="context-data">
<node CREATED="1494332400568" ID="ID_1706198485" MODIFIED="1494332405164" TEXT="*anything*">
<node CREATED="1494332406120" ID="ID_156197229" MODIFIED="1494332426108" TEXT="does not propogate to the next block"/>
</node>
</node>
<node CREATED="1494332434968" ID="ID_592370413" MODIFIED="1494332437669" TEXT="code">
<node CREATED="1494332440569" ID="ID_679539513" MODIFIED="1494332448060" TEXT="current root chain code">
<node CREATED="1494332449176" ID="ID_1299808518" MODIFIED="1494332595403" TEXT="note that we put the entire coding here. We don&apos;t make the clients run through whatever passed proposals happened in earlier blocks and process the corresponding diffs.&#xa;&#xa;This is so that only the top level structure of the auth hash map has to be pulled in for each block, and the client can race ahead to the last one and get everything they need to run the root chain"/>
</node>
</node>
<node CREATED="1494332600513" ID="ID_136964778" MODIFIED="1494332603613" TEXT="prior block hash"/>
<node CREATED="1494332606848" ID="ID_1744540569" MODIFIED="1494332608716" TEXT="timestamp"/>
<node CREATED="1494332609264" ID="ID_14839999" MODIFIED="1494483820994" TEXT="sub context genesii">
<node CREATED="1494483820974" ID="ID_213448999" MODIFIED="1494483823948" TEXT="idea A">
<node CREATED="1494332619105" ID="ID_114569554" MODIFIED="1494332631332" TEXT="This is the code for sub context genesis block"/>
<node CREATED="1494332631728" ID="ID_876387244" MODIFIED="1494332638692" TEXT="We don&apos;t name these, they are references by index">
<node CREATED="1494332640000" ID="ID_1574860152" MODIFIED="1494332652812" TEXT="This is to prevent name squatting, typo errors, etc."/>
<node CREATED="1494332653976" ID="ID_1153427560" MODIFIED="1494332670916" TEXT="Context names will be associated out of band (on websites, etc)"/>
<node CREATED="1494483936295" ID="ID_156981020" MODIFIED="1494483955091" TEXT="A name directory sub context could also handle context names"/>
</node>
</node>
<node CREATED="1494483973328" ID="ID_549044539" MODIFIED="1494483975051" TEXT="idea B">
<node CREATED="1494483975456" ID="ID_1607122972" MODIFIED="1494483981659" TEXT="we do name these subcontexts"/>
</node>
<node CREATED="1494483996543" ID="ID_184085335" MODIFIED="1494484021123" TEXT="sub contexts stay in the tree for only so long????">
<node CREATED="1494483842257" ID="ID_1744820894" MODIFIED="1494483868780" TEXT="This forces the sub context to re-register itself periodically (akin to ICANN), by getting a miner to include it in a block"/>
</node>
</node>
</node>
</node>
<node CREATED="1494494195243" FOLDED="true" ID="ID_1445040979" MODIFIED="1497252353879" TEXT="5/11/17">
<node CREATED="1494494207543" ID="ID_1848661477" MODIFIED="1494494216080" TEXT="simplest genesis is best">
<node CREATED="1494494276594" ID="ID_638916825" MODIFIED="1494494288462" TEXT="set data hash">
<node CREATED="1494494289922" ID="ID_1262079448" MODIFIED="1494494296774" TEXT="hash of something/anything, for each block"/>
</node>
<node CREATED="1494494300850" ID="ID_1798042764" MODIFIED="1494494302209" TEXT="nonce"/>
<node CREATED="1494494302551" ID="ID_1749687635" MODIFIED="1494494305496" TEXT="timestamp"/>
<node CREATED="1494494305892" ID="ID_983232074" MODIFIED="1494494311320" TEXT="root code"/>
<node CREATED="1494494218999" ID="ID_1162305189" MODIFIED="1494494322647" TEXT="run_context">
<node CREATED="1494494228713" ID="ID_1861330465" MODIFIED="1494494231963" TEXT="given code"/>
<node CREATED="1494494232220" ID="ID_1761804354" MODIFIED="1494494236209" TEXT="runnable by anything"/>
<node CREATED="1494494236651" ID="ID_29589481" MODIFIED="1494494241552" TEXT="problem">
<node CREATED="1494494243217" ID="ID_1195199715" MODIFIED="1494494248763" TEXT="what about starvation?">
<node CREATED="1494494250858" ID="ID_936879323" MODIFIED="1494499572284" TEXT="a sub context could start running code program after running code program, starving the system of resources"/>
</node>
</node>
</node>
</node>
</node>
<node CREATED="1494652625246" FOLDED="true" ID="ID_948099209" MODIFIED="1497252354889" TEXT="5/13/17">
<node CREATED="1494652631454" ID="ID_1168652584" MODIFIED="1494652709069" TEXT="starvation and the general idea">
<node CREATED="1494652638390" ID="ID_1968870603" MODIFIED="1494652679746" TEXT="One of the best features, in my mind, is the ability for the network to assign specific tasks to individual clients"/>
<node CREATED="1494652709063" ID="ID_1197193175" MODIFIED="1494652712346" TEXT="starvation">
<node CREATED="1494652681622" ID="ID_1926771573" MODIFIED="1494652699890" TEXT="I worry about &quot;starvation&quot;, by having a sub context use too many resources of the user"/>
</node>
<node CREATED="1494652712846" ID="ID_819080371" MODIFIED="1494652720738" TEXT="role of type theory">
<node CREATED="1494652721966" ID="ID_1592691083" MODIFIED="1494652747674" TEXT="suppose there was no type theory, could we still get this to be useful using imperative programming?"/>
<node CREATED="1494652747942" ID="ID_1835101460" MODIFIED="1494652850290" TEXT="if we just had a collection of sandboxes for each context, that is"/>
</node>
</node>
<node CREATED="1494652875942" ID="ID_523488894" MODIFIED="1495178679417" TEXT="simple coin subcontext">
<node CREATED="1494653096317" ID="ID_1628125360" MODIFIED="1494658550240" TEXT="Is there a difference between recording a history and validating it?">
<node CREATED="1494654669148" ID="ID_391952214" MODIFIED="1494654701954" TEXT="Can we record a history in a block chain, right or wrong, and deal with it later when we need to validate it?"/>
<node CREATED="1494654704246" ID="ID_882056342" MODIFIED="1494654718307" TEXT="Suppose we just store &quot;data&quot; into the chain, which could be anything">
<node CREATED="1494654719239" ID="ID_87960926" MODIFIED="1494654729931" TEXT="Since it could be anything, it doesn&apos;t need to be validated, and can be just a hash"/>
<node CREATED="1494654734671" ID="ID_1963938198" MODIFIED="1494654780355" TEXT="If we want to validate the funds of a user, we expect a proof, which would be the data from the chain, showing the transactions that led to a particular balance"/>
<node CREATED="1494654782255" ID="ID_61376368" MODIFIED="1494654830546" TEXT="The only issue here is that we have to make sure that a balance can&apos;t be considered valid if some transactions removing funds are left out."/>
<node CREATED="1494654859127" ID="ID_1464802232" MODIFIED="1494654867459" TEXT="This seems to be akin to the double spending problem"/>
</node>
<node CREATED="1494656235624" ID="ID_735624888" MODIFIED="1494656241148" TEXT="I think we can do the following">
<node CREATED="1494656242240" ID="ID_1499326163" MODIFIED="1494656264860" TEXT="Each block contains the *balance* of all accounts on the system"/>
<node CREATED="1494656269288" ID="ID_1893772115" MODIFIED="1494656297044" TEXT="Since normally, balances don&apos;t change, the merkle hash from one block to the next doesn&apos;t change much"/>
<node CREATED="1494656301672" ID="ID_672794671" MODIFIED="1494656310316" TEXT="Therefore the problem is solved."/>
</node>
<node CREATED="1494656364000" ID="ID_625674618" MODIFIED="1494656367499" TEXT="garbage data is allowed"/>
<node CREATED="1494656354432" ID="ID_36048700" MODIFIED="1494656361868" TEXT="The owner then keeps track of their own data">
<node CREATED="1494656369104" ID="ID_1965242143" MODIFIED="1494656373924" TEXT="For example">
<node CREATED="1494656381935" ID="ID_833106996" MODIFIED="1494656391620" TEXT="Account x has $0"/>
<node CREATED="1494656391952" ID="ID_1470984489" MODIFIED="1494656426140" TEXT="A provably inconsistent transaction is published for account x"/>
<node CREATED="1494656426464" ID="ID_1599734835" MODIFIED="1494656500395" TEXT="Any balance change associated to the inconsistent transaction is ignored"/>
<node CREATED="1494656500696" ID="ID_1113403670" MODIFIED="1494656545940" TEXT="Anyone that wanted to really transact against account x would know this when auditing data for account x"/>
<node CREATED="1494656546624" ID="ID_1679034909" MODIFIED="1494656550628" TEXT="And could publish a duplicate"/>
</node>
</node>
<node CREATED="1494657542200" ID="ID_951614053" MODIFIED="1494657555300" TEXT="Problem is that an account could be bombed with garbage data">
<node CREATED="1494657556728" ID="ID_1112610482" MODIFIED="1494657585396" TEXT="Suppose someone created a random 10 Terabyte file, hashed it, and placed it into the tree as a transaction for a users account"/>
<node CREATED="1494657587065" ID="ID_1302711710" MODIFIED="1494657608740" TEXT="The user would have to prove that the data is garbage, and the only way they could do that is get a copy of the data"/>
<node CREATED="1494657611896" ID="ID_755992793" MODIFIED="1494657617476" TEXT="First, they wouldn&apos;t have it."/>
<node CREATED="1494657619136" ID="ID_334145680" MODIFIED="1494657901413" TEXT="Second, even if they did, no one would accept the proof that it was invalid because it would take too long to calculate"/>
</node>
<node CREATED="1494658117032" ID="ID_464611604" MODIFIED="1494658147301" TEXT="What if the data must be signed by the account holder to get into the tree">
<node CREATED="1494658165961" ID="ID_1005776767" MODIFIED="1494658187724" TEXT="Then each signature would have to be kept by the miners"/>
<node CREATED="1494658189624" ID="ID_984943375" MODIFIED="1494658220173" TEXT="Otherwise, if there is an entry, how to prove that it is not fake (if it was not checked by every miner for every entry)"/>
<node CREATED="1494658232681" ID="ID_1985289115" MODIFIED="1494658307165" TEXT="What if a signature could be added to a set of data items?"/>
<node CREATED="1494658307601" ID="ID_1387184955" MODIFIED="1494658350045" TEXT="A root hash of a merkle tree of all the data signed by that signature?"/>
<node CREATED="1494658361161" ID="ID_1346605041" MODIFIED="1494658373173" TEXT="And again, just data itself."/>
<node CREATED="1494658374521" ID="ID_1047044521" MODIFIED="1494658392469" TEXT="A merklized hash table">
<node CREATED="1494658393329" ID="ID_133067375" MODIFIED="1494658405229" TEXT="Public key as hash table key"/>
<node CREATED="1494658405617" ID="ID_1978507103" MODIFIED="1494658423965" TEXT="All signed data ever for that key as the data"/>
</node>
</node>
</node>
</node>
</node>
<node CREATED="1495178698942" FOLDED="true" ID="ID_272726938" MODIFIED="1495788160221" TEXT="5/19/17">
<node CREATED="1495178713322" ID="ID_1442727301" MODIFIED="1495178975634" TEXT="avoiding excessive data validation">
<node CREATED="1495178747538" ID="ID_535448253" MODIFIED="1495178760253" TEXT="i&apos;d like to avoid every node checking every other nodes transactions all the time"/>
<node CREATED="1495178914298" ID="ID_517677413" MODIFIED="1495178944870" TEXT="if there was a way to write some kind of overriding final balance between different entities, that could be signed by everyone"/>
<node CREATED="1495178721370" ID="ID_1524756811" MODIFIED="1495178728646" TEXT="what about ripplenomic?"/>
<node CREATED="1495178957202" ID="ID_1251090853" MODIFIED="1495179118058" TEXT="orig idea would be to allow clients to write anything to the log">
<node CREATED="1495178982050" ID="ID_1508940508" MODIFIED="1495178998214" TEXT="and other clients that needed a particular clients balance could calculate it on the fly"/>
<node CREATED="1495178999306" ID="ID_895706914" MODIFIED="1495179010054" TEXT="problem is that the clients wallet would keep on increasing in size">
<node CREATED="1495179013666" ID="ID_1336108773" MODIFIED="1495179038974" TEXT="and a 5cent transfer from a 5 gig wallet would make a 5 gig wallet + 10 bytes or something"/>
<node CREATED="1495179139139" ID="ID_554816471" MODIFIED="1495179156047" TEXT="and wallet sizes would multiply with each transaction between each other"/>
</node>
<node CREATED="1495179254739" ID="ID_1610155114" MODIFIED="1495179267263" TEXT="if server was dumb, just accepted all data">
<node CREATED="1495179268475" ID="ID_1943202406" MODIFIED="1495179276599" TEXT="then to validate, all data must be gone through"/>
</node>
</node>
</node>
<node CREATED="1495187318849" ID="ID_1456278502" MODIFIED="1495187326013" TEXT="context summarization">
<node CREATED="1495187327393" ID="ID_1865581578" MODIFIED="1495187343844" TEXT="a context could be the domain of transfers between two individuals"/>
<node CREATED="1495187344929" ID="ID_1276118172" MODIFIED="1495187387629" TEXT="it can have a clause that states with both signatures, the result can overwrite the transactions within it">
<node CREATED="1495187389681" ID="ID_111487190" MODIFIED="1495187435165" TEXT="thereby removing some data that to prove the balance of a wallet"/>
</node>
</node>
<node CREATED="1495187438017" ID="ID_899922339" MODIFIED="1495187516598" TEXT="are subcontexts the same as my idea of historical logs?">
<node CREATED="1495187454137" ID="ID_857893949" MODIFIED="1495187474621" TEXT="My idea that data stored on root would contain a link to a past entry, thereby producing a chain">
<node CREATED="1495187478161" ID="ID_1848522040" MODIFIED="1495187493685" TEXT="this could be used as proof of being able to get all the entries for a particular domain"/>
<node CREATED="1495187493953" ID="ID_797487632" MODIFIED="1495187509341" TEXT="where a domain could be all the transactions from a single party, between two different parties, etc"/>
</node>
</node>
</node>
<node CREATED="1495270860523" FOLDED="true" ID="ID_1936400667" MODIFIED="1506491627544" TEXT="5/20/17">
<node CREATED="1495270876140" ID="ID_326620537" MODIFIED="1495270881215" TEXT="dealing with wallet bloat">
<node CREATED="1495270885571" ID="ID_1628252285" MODIFIED="1495270922463" TEXT="when funds are transferred, each receiver gets a special key">
<node CREATED="1495270923972" ID="ID_822808633" MODIFIED="1495270975192" TEXT="When enough special keys are recovered to represent the entirety of the outgoing funds (probably always 2), by the same wallet, or a combination of wallets">
<node CREATED="1495270976300" ID="ID_1833203219" MODIFIED="1495270999279" TEXT="a summary transaction can be written to stand in place of all the intervening ones"/>
</node>
</node>
</node>
<node CREATED="1495271003316" ID="ID_6173293" MODIFIED="1497307586008" TEXT="open phase">
<node CREATED="1495271007084" ID="ID_1120887975" MODIFIED="1495271016895" TEXT="open phase code is just data, like anything else">
<node CREATED="1495271019372" ID="ID_1652747705" MODIFIED="1495271028776" TEXT="it runs in a special sandbox setup by the client">
<node CREATED="1495271029668" ID="ID_1554984797" MODIFIED="1495271052920" TEXT="the sandbox can be setup to ask for certain proofs before running code"/>
<node CREATED="1495271087716" ID="ID_184531111" MODIFIED="1495271120296" TEXT="some important rules might be to never use sensitive data in a direct manner">
<node CREATED="1495271121236" ID="ID_1927304066" MODIFIED="1495271164208" TEXT="(open phase code can never sign transactions, even if given a secret by a user, for example)"/>
<node CREATED="1495271171660" ID="ID_1264031190" MODIFIED="1495271211889" TEXT="some of this will be user education (ex, never type in your password into a given window marked &quot;sandbox&quot;"/>
</node>
</node>
</node>
</node>
</node>
<node CREATED="1495697555950" FOLDED="true" ID="ID_510281504" MODIFIED="1506493528471" TEXT="5/25/17">
<node CREATED="1495697561972" ID="ID_553150276" MODIFIED="1495697580960" TEXT="why should it be that the restrictions needed to run open phase code be hard coded?"/>
<node CREATED="1495697581500" ID="ID_1561198049" MODIFIED="1495697596808" TEXT="what is the point of it?">
<node CREATED="1495697597772" ID="ID_906652786" MODIFIED="1495697612840" TEXT="why not use your own code, for example..."/>
</node>
</node>
<node CREATED="1495789217617" FOLDED="true" ID="ID_358669585" MODIFIED="1506493529485" TEXT="5/26/17">
<node CREATED="1495789224947" ID="ID_735170634" MODIFIED="1495789279534" TEXT="suppose we had the simplist possible system">
<node CREATED="1495789239658" ID="ID_295752370" MODIFIED="1495789261246" TEXT="only necessary to run agda code and be able to run processes, nothing else"/>
<node CREATED="1495789280777" ID="ID_827110219" MODIFIED="1495789298669" TEXT="the inner core would start up a program to get the blocks, etc"/>
<node CREATED="1495789299898" ID="ID_129610100" MODIFIED="1495789315510" TEXT="any code that &quot;can&apos;t change&quot; within the inner core would be unchangeable"/>
</node>
<node CREATED="1495789351026" ID="ID_185675039" MODIFIED="1495789388822" TEXT="it would need to store a lot of data accessible by the inner core">
<node CREATED="1495789390689" ID="ID_897550998" MODIFIED="1495789399566" TEXT="agda against a database, I suppose"/>
</node>
</node>
<node CREATED="1497045165423" FOLDED="true" ID="ID_877590802" MODIFIED="1506491629518" TEXT="6/10/17">
<node CREATED="1497045168985" ID="ID_159501110" MODIFIED="1497084036007" TEXT="agda seems too slow">
<node CREATED="1497084037836" ID="ID_1218087261" MODIFIED="1497084120973">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      Just to compile with some basic libraries,
    </p>
    <p>
      
    </p>
    <p>
      Data.Nat using (N)
    </p>
    <p>
      Data.Integer using (Z)
    </p>
    <p>
      Data.Fin using (Fin)
    </p>
    <p>
      
    </p>
    <p>
      takes several seconds
    </p>
    <p>
      
    </p>
    <p>
      I can change to the Agda builtins but is seems troublesome. There is also some doc on the web about this.
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1497084121908" ID="ID_746002564" MODIFIED="1497084145872" TEXT="I&apos;m thinking that I really need to learn the basics of what the agda compiler does"/>
<node CREATED="1497084146164" ID="ID_1132224511" MODIFIED="1497084186928" TEXT="I will start with simpler easier"/>
<node CREATED="1497084188381" ID="ID_1667483398" MODIFIED="1497084198680" TEXT="These are the components of agda that I know about">
<node CREATED="1497084199676" ID="ID_153130229" MODIFIED="1497084204136" TEXT="dependent type checking"/>
<node CREATED="1497084204444" ID="ID_1472488053" MODIFIED="1497084208857" TEXT="pattern matching">
<node CREATED="1497084209683" ID="ID_1039007404" MODIFIED="1497084215744" TEXT="including totality checker"/>
</node>
<node CREATED="1497084216724" ID="ID_903101661" MODIFIED="1497084250728" TEXT="unification"/>
</node>
<node CREATED="1497084262781" ID="ID_392851508" MODIFIED="1497084284911" TEXT="I want to see if I can write a compiler to parse a simplified version of agda, then use that to develop the Autonomic Language"/>
</node>
<node CREATED="1498695082776" ID="ID_824045169" MODIFIED="1498695089562" TEXT="(idris is slow as well, though)"/>
</node>
<node CREATED="1498544467443" FOLDED="true" ID="ID_444393920" MODIFIED="1506491630196" TEXT="6/27/17">
<node CREATED="1498544472071" ID="ID_1142713155" MODIFIED="1498544561980" TEXT="equality is conversion">
<node CREATED="1498544563041" ID="ID_1730600607" MODIFIED="1498544574980" TEXT="suppose I have plus defined as ">
<node CREATED="1498544576088" ID="ID_1240739459" MODIFIED="1498544618563">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      plus : Nat -&gt; Nat -&gt; Nat
    </p>
    <p>
      Z x = x
    </p>
    <p>
      (S x) y = S (plus (x y))
    </p>
  </body>
</html></richcontent>
</node>
</node>
<node CREATED="1498544624697" ID="ID_1668309798" MODIFIED="1498544636173" TEXT="it works, but it&apos;s quite slow for large numbers"/>
<node CREATED="1498544636505" ID="ID_1544720377" MODIFIED="1498544713421" TEXT="Now lets say that I have functions to convert back and forth between basic and binary representation"/>
<node CREATED="1498544718976" ID="ID_1459224071" MODIFIED="1498544756196" TEXT="Then I should be able to prove any function involving nats, applies to Bins as well"/>
<node CREATED="1498544763057" ID="ID_1761501153" MODIFIED="1498544772372" TEXT="But it&apos;d still be slow">
<node CREATED="1498544784832" ID="ID_1072308480" MODIFIED="1498544821244" TEXT="If we take a straight function for Nats, such as the above &quot;plus&quot;, then it will still act in its very slow way to add numbers"/>
<node CREATED="1498544831464" ID="ID_445983710" MODIFIED="1498544839428" TEXT="Of course we can prove a faster version of plus"/>
</node>
<node CREATED="1498544892704" ID="ID_1619921601" MODIFIED="1498544941516" TEXT="So to pass a Nat, we could pass instead (toNat binX) where binX is a binary representation"/>
<node CREATED="1498544985392" ID="ID_1819987495" MODIFIED="1498544985392" TEXT=""/>
</node>
</node>
<node CREATED="1498695275220" FOLDED="true" ID="ID_1982420065" MODIFIED="1506491630973" TEXT="6/29/17">
<node CREATED="1498695280363" ID="ID_261551996" MODIFIED="1498695311710" TEXT="inductive proof and totality checking">
<node CREATED="1498695293467" ID="ID_1972476799" MODIFIED="1498695432366" TEXT="ex">
<node CREATED="1498695433346" ID="ID_793291413" MODIFIED="1498695494675">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      &#160;&#160;div2e : (n : &#8469;) &#8594; even n &#8594; &#8469; -- Note, we have to give a name `n` to the first argument here
    </p>
    <p>
      &#160;&#160;div2e zero p = zero
    </p>
    <p>
      &#160;&#160;div2e (succ zero) ()
    </p>
    <p>
      &#160;&#160;div2e (succ (succ y)) p = succ (div2e y p) -- Note, a proof of `even (succ (succ n))` translates
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- to a proof of `even n` by the definition of `even`.
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1498695500042" ID="ID_1914157676" MODIFIED="1498695529606" TEXT="How does the totality checker handle multiple inductive base cases?"/>
</node>
</node>
</node>
<node CREATED="1503971122856" FOLDED="true" ID="ID_1964726340" MODIFIED="1506491631483" TEXT="8/29/17">
<node CREATED="1503971138143" ID="ID_1364897616" MODIFIED="1503971308898" TEXT="data items in agda">
<node CREATED="1503971310229" ID="ID_272405922" MODIFIED="1503971337697" TEXT="A data item is an undefined function, with certain restrictions"/>
<node CREATED="1503971339214" ID="ID_1615688104" MODIFIED="1503971372105" TEXT="You can only supply this &quot;data&quot; function if you are a member within its &quot;where&quot; clause"/>
</node>
</node>
<node CREATED="1506491632237" FOLDED="true" ID="ID_957196088" MODIFIED="1507201210574" TEXT="9/27/17">
<node CREATED="1506491637938" ID="ID_1255666149" MODIFIED="1506491653341" TEXT="write system in idris to get practice with dependent types"/>
<node CREATED="1506491654483" ID="ID_527718369" MODIFIED="1506491666671" TEXT="core system will have following">
<node CREATED="1506491667419" ID="ID_97738476" MODIFIED="1506491672127" TEXT="interpreter">
<node CREATED="1506491709842" ID="ID_1969882244" MODIFIED="1506491722159" TEXT="allows code to specify modules to add to itself"/>
</node>
<node CREATED="1506491672939" ID="ID_565842790" MODIFIED="1506491699399" TEXT="bridge for code to run anything it likes"/>
</node>
<node CREATED="1506491726172" ID="ID_1177715759" MODIFIED="1506491736294" TEXT="the rest will be stored in &quot;rom&quot; in the target language">
<node CREATED="1506491738235" ID="ID_8400831" MODIFIED="1506491748783" TEXT="This allows for simpler code outside of the main construct"/>
<node CREATED="1506491749050" ID="ID_1516055053" MODIFIED="1506491765215" TEXT="I think this will be a benefit to directly address certain things within the system"/>
</node>
<node CREATED="1506666823974" ID="ID_181493864" MODIFIED="1506666841618" TEXT="what if functions that aren&apos;t total can be treated like data types">
<node CREATED="1506666843037" ID="ID_783066693" MODIFIED="1506666863777" TEXT="Right now, we say that &quot;data X = A | B&quot; means">
<node CREATED="1506666865900" ID="ID_1382588974" MODIFIED="1506666888443">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      A : X&#160;&#160;
    </p>
    <p>
      B : X
    </p>
    <p>
      
    </p>
    <p>
      (where A and B aren't defined)
    </p>
  </body>
</html></richcontent>
</node>
</node>
<node CREATED="1506666889653" ID="ID_1380167812" MODIFIED="1506666925177" TEXT="What if we say that functions can be partially defined, and still treated somewhat like data types?"/>
<node CREATED="1506666934549" ID="ID_740636747" MODIFIED="1506666952321" TEXT="Con: This would be fine, but the point of data is to limit the ability to make non-total functions">
<node CREATED="1506666959830" ID="ID_1056955187" MODIFIED="1506666987457" TEXT="You can only make non total functions that return a particular type if they are in &quot;data &lt;type&gt;&quot;"/>
</node>
</node>
<node CREATED="1506687613914" ID="ID_1719431884" MODIFIED="1506947143831" TEXT="idris features">
<node CREATED="1506687617474" ID="ID_475150420" MODIFIED="1506687620454" TEXT="implied variables">
<node CREATED="1506910449352" ID="ID_1655928445" MODIFIED="1506910463973" TEXT="including implied variables that aren&apos;t defined in definition ">
<node CREATED="1506910465169" ID="ID_864627048" MODIFIED="1506910466524" TEXT="ex">
<node CREATED="1506910468073" ID="ID_1974961267" MODIFIED="1506910478778">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      data SnocList : List a -&gt; Type where
    </p>
    <p>
      &#160;Empty :&#160;&#160;SnocList []
    </p>
    <p>
      &#160;Snoc : (rec : SnocList xs) -&gt; SnocList (xs ++ [x])
    </p>
  </body>
</html></richcontent>
<node CREATED="1506910483056" ID="ID_43602627" MODIFIED="1506910494028" TEXT="x is not defined, but is an implied variable"/>
</node>
</node>
</node>
</node>
<node CREATED="1506687620939" ID="ID_1902236482" MODIFIED="1506687623446" TEXT="data types"/>
<node CREATED="1506687623746" ID="ID_1593470981" MODIFIED="1506687625510" TEXT="records"/>
<node CREATED="1506687625963" ID="ID_603713278" MODIFIED="1506687807884" TEXT="dependent tuples">
<node CREATED="1506687634106" ID="ID_412066874" MODIFIED="1506687639374" TEXT="( x ** y )"/>
<node CREATED="1506760428347" ID="ID_1832547117" MODIFIED="1506760431198" TEXT="(newLength ** Vect newLength a)"/>
</node>
<node CREATED="1506687811609" ID="ID_182572548" MODIFIED="1506687820949" TEXT="pattern unification ( of course)"/>
<node CREATED="1506687861209" ID="ID_1787272596" MODIFIED="1506687863109" TEXT="case statements"/>
<node CREATED="1506687863385" ID="ID_475568993" MODIFIED="1506687865117" TEXT="with statements">
<node CREATED="1506930506319" ID="ID_1866549462" MODIFIED="1506930511122" TEXT="recursive calls on the right side">
<node CREATED="1506930512478" ID="ID_203973080" MODIFIED="1506930544257">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      data SnocList : List a -&gt; Type where
    </p>
    <p>
      &#160;Empty :&#160;&#160;SnocList []
    </p>
    <p>
      &#160;Snoc : (rec : SnocList xs) -&gt; SnocList (xs ++ [x])
    </p>
    <p>
      &#160;
    </p>
    <p>
      snocListHelp : (snoc : SnocList input) -&gt; (rest : List a) -&gt; SnocList (input ++ rest)
    </p>
    <p>
      snocListHelp {input} snoc [] =&#160;&#160;rewrite appendNilRightNeutral input in snoc
    </p>
    <p>
      snocListHelp {input} snoc (x :: xs) =
    </p>
    <p>
      &#160;&#160;let
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;slhRest = snocListHelp (Snoc snoc {x}) xs
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;in rewrite appendAssociative input [x] xs in slhRest
    </p>
    <p>
      
    </p>
    <p>
      snocList : (xs : List a) -&gt; SnocList xs
    </p>
    <p>
      snocList xs = snocListHelp Empty xs
    </p>
    <p>
      
    </p>
    <p>
      myReverse2 : List a -&gt; List a
    </p>
    <p>
      myReverse2 input with (snocList input)
    </p>
    <p>
      &#160;&#160;myReverse2 [] | Empty = []
    </p>
    <p>
      &#160;&#160;myReverse2 (xs ++ [x]) | (Snoc rec) = x :: myReverse2 xs | rec
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1506930589671" ID="ID_1098837911" MODIFIED="1506930599026" TEXT="&quot;In practice, when you use the with construct, Idris introduces a new function defini- tion for the body of the with block, like the definition of myReverseHelper that you implemented manually earlier&quot;"/>
</node>
</node>
<node CREATED="1506687865441" ID="ID_1082026090" MODIFIED="1506687866245" TEXT="holes"/>
<node CREATED="1506687866544" ID="ID_1364418183" MODIFIED="1506687911686" TEXT="list syntactic sugar">
<node CREATED="1506687892273" ID="ID_1022681844" MODIFIED="1506687897429" TEXT="[ 1,2 .. 3]"/>
</node>
<node CREATED="1506687932337" ID="ID_1323123169" MODIFIED="1506687934805" TEXT=":printdef"/>
<node CREATED="1506687936081" ID="ID_1311677525" MODIFIED="1506687936901" TEXT=":doc"/>
<node CREATED="1506687937921" ID="ID_1835430721" MODIFIED="1506687939452" TEXT=":type"/>
<node CREATED="1506688584176" ID="ID_451120727" MODIFIED="1506688590940" TEXT="pattern matching on arguments"/>
<node CREATED="1506688591375" ID="ID_235267527" MODIFIED="1506688594380" TEXT="&quot;impossible&quot; tag"/>
<node CREATED="1506688676472" ID="ID_1964050682" MODIFIED="1506688678756" TEXT="let ..."/>
<node CREATED="1506688679031" ID="ID_1508425933" MODIFIED="1506688680292" TEXT="do ..."/>
<node CREATED="1506689061207" ID="ID_1211273238" MODIFIED="1506689062811" TEXT="rewrite">
<node CREATED="1506727174519" ID="ID_1494967661" MODIFIED="1506727197035" TEXT="rewrite figures out which sub expression to rewrite to match expected type">
<node CREATED="1506727198111" ID="ID_1450332320" MODIFIED="1506727199219" TEXT="ex">
<node CREATED="1506727200271" ID="ID_5520977" MODIFIED="1506727238611" TEXT="myPlusCommutes : (n : Nat) -&gt; (m : Nat) -&gt; n + m = m + n&#xa;myPlusCommutes Z m = rewrite sym (plusZeroRightNeutral m) in Refl"/>
<node CREATED="1506727241127" ID="ID_909319448" MODIFIED="1506727251347" TEXT="myPlusCommutes : (n : Nat) -&gt; (m : Nat) -&gt; n + m = m + n &#xa;myPlusCommutes Z m = rewrite (plusZeroRightNeutral m) in Refl "/>
<node CREATED="1506727253055" ID="ID_920202176" MODIFIED="1506729445336" TEXT="Both the above work, because the &quot;m&quot; that is rewritten in (Refl {Z+m} : (Z + m = Z + m)) changes"/>
</node>
</node>
<node CREATED="1506727966686" ID="ID_1366072671" MODIFIED="1506727973714" TEXT="without an expected type. rewrite fails">
<node CREATED="1506727975230" ID="ID_585312111" MODIFIED="1506727975826" TEXT="ex">
<node CREATED="1506727976221" ID="ID_776141190" MODIFIED="1506728013601" TEXT="myPlusCommutes : (n : Nat) -&gt; (m : Nat) -&gt; n + m = m + n&#xa;myPlusCommutes Z m = rewrite sym (plusZeroRightNeutral m) in Refl&#xa;myPlusCommutes (S k) m = let r = Refl {x = k + m}&#xa;                             pc = myPlusCommutes k m&#xa;                             pr = plusSuccRightSucc m k&#xa;                             in rewrite pc in pr&#xa;&#xa;"/>
<node CREATED="1506728017782" ID="ID_280148711" MODIFIED="1506728221420">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      myPlusCommutes : (n : Nat) -&gt; (m : Nat) -&gt; n + m = m + n
    </p>
    <p>
      myPlusCommutes Z m = rewrite sym (plusZeroRightNeutral m) in Refl
    </p>
    <p>
      myPlusCommutes (S k) m = let r = Refl {x = k + m}
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;pc = myPlusCommutes k m
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;pr = plusSuccRightSucc m k
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;res = rewrite pc in pr
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;in ?myPlusCommutes_rhs_2
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1506728040374" ID="ID_633501674" MODIFIED="1506728054538" TEXT="Second one fails, because idris doesn&apos;t know what type to expect for res"/>
</node>
</node>
</node>
<node CREATED="1506726364097" ID="ID_165957297" MODIFIED="1506726370500" TEXT="where clauses in functions"/>
<node CREATED="1506750368152" ID="ID_295863889" MODIFIED="1506750370867" TEXT="custom tactics"/>
<node CREATED="1506761859192" ID="ID_1761556484" MODIFIED="1506761860412" TEXT="auto">
<node CREATED="1506761861537" ID="ID_355504945" MODIFIED="1506761873317" TEXT="idris tries to automatically find an argument for something">
<node CREATED="1506761875025" ID="ID_1127901896" MODIFIED="1506761924805">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      removeElem_auto : (value : a) -&gt; (xs : Vect (S n) a) -&gt;
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;{auto prf : Elem value xs} -&gt; Vect n a
    </p>
    <p>
      removeElem_auto value xs {prf} = removeElem value xs prf
    </p>
  </body>
</html></richcontent>
</node>
</node>
</node>
<node CREATED="1506947140089" ID="ID_313394768" MODIFIED="1506947141760" TEXT="modules">
<node CREATED="1506947146847" ID="ID_460470272" MODIFIED="1506947148851" TEXT="imports"/>
<node CREATED="1506947149258" ID="ID_253352656" MODIFIED="1506947150617" TEXT="exports"/>
<node CREATED="1507000438243" ID="ID_711118863" MODIFIED="1507000447567" TEXT="public export"/>
</node>
<node CREATED="1507014972779" ID="ID_1205461959" MODIFIED="1507014973919" TEXT="Inf">
<node CREATED="1507014975427" ID="ID_1911103918" MODIFIED="1507014977438" TEXT="Delay"/>
<node CREATED="1507014978011" ID="ID_233514130" MODIFIED="1507014979807" TEXT="Force"/>
<node CREATED="1507014980082" ID="ID_1676326604" MODIFIED="1507014996119" TEXT="Delay is always considered total"/>
</node>
</node>
<node CREATED="1506687828033" ID="ID_948502540" MODIFIED="1506687845717" TEXT="idris book states that idris needs definitions in order to support dependent types">
<node CREATED="1506687846873" ID="ID_762678498" MODIFIED="1506687848229" TEXT="why?"/>
</node>
</node>
<node CREATED="1506842571639" FOLDED="true" ID="ID_1494183749" MODIFIED="1507201213101" TEXT="10/1/17">
<node CREATED="1506842576975" ID="ID_131738959" MODIFIED="1506842593651" TEXT="can we use non-total functions rather than a vm?">
<node CREATED="1506842594759" ID="ID_872941322" MODIFIED="1506842597772" TEXT="I don&apos;t think so"/>
<node CREATED="1506842601063" ID="ID_369434404" MODIFIED="1506842618259" TEXT="Non-total functions can&apos;t be told how much memory they can use">
<node CREATED="1506842621719" ID="ID_1009478174" MODIFIED="1506842638979" TEXT="A non-total function could create a huge array internally, with no way to stop it"/>
</node>
</node>
</node>
<node CREATED="1507770544670" ID="ID_1750016855" MODIFIED="1507770547704" TEXT="10/12/17">
<node CREATED="1507770548269" ID="ID_1233944592" MODIFIED="1507770552689" TEXT="side channels are dangerous">
<node CREATED="1507770553884" ID="ID_90082777" MODIFIED="1507770558296" TEXT="record mouse movements, etc.">
<node CREATED="1507770564342" ID="ID_1252809634" MODIFIED="1507770575048" TEXT="send this data to some external entitiy"/>
</node>
</node>
</node>
<node CREATED="1508028126716" FOLDED="true" ID="ID_346940689" MODIFIED="1510030843680" TEXT="10/14/17">
<node CREATED="1508028132394" ID="ID_1734455036" MODIFIED="1508028164053" TEXT="I want to try out dependent types and make a bunch of basic atomic rules that the implementation will have to follow"/>
<node CREATED="1508028165234" ID="ID_140762470" MODIFIED="1508028170117" TEXT="There should be more than one set">
<node CREATED="1508028171362" ID="ID_942278922" MODIFIED="1508028179829" TEXT="A set to interpret an AST"/>
<node CREATED="1508028180361" ID="ID_1320137725" MODIFIED="1508028191894" TEXT="A set that converts written text to an AST"/>
</node>
<node CREATED="1508028192977" ID="ID_1267997520" MODIFIED="1508028209789" TEXT="This could be one set, but that would flatten the code out too much and make things hard to follow"/>
<node CREATED="1508033301028" ID="ID_529665960" MODIFIED="1508060103689" TEXT="Why is type of Lam Pi, but the type of Pi is just the type of its body?"/>
<node CREATED="1508060111875" ID="ID_618182340" MODIFIED="1508060118614" TEXT="try 1">
<node CREATED="1508060119779" ID="ID_993206215" MODIFIED="1508060124111" TEXT="RawContext">
<node CREATED="1508060125202" ID="ID_1727903764" MODIFIED="1508060132118" TEXT="just a record, no restrictions"/>
<node CREATED="1508060132554" ID="ID_1228330669" MODIFIED="1508060133543" TEXT="contains">
<node CREATED="1508060134450" ID="ID_264596104" MODIFIED="1508060176647" TEXT="vars as name to RawContext">
<node CREATED="1508060142958" ID="ID_951357874" MODIFIED="1508060148935" TEXT="where name is a configurable type"/>
<node CREATED="1508060177627" ID="ID_747186611" MODIFIED="1508060185999" TEXT="should the value really be another RawContext?"/>
</node>
<node CREATED="1508060188002" ID="ID_893741525" MODIFIED="1508060197182" TEXT="return type">
<node CREATED="1508060203531" ID="ID_445510156" MODIFIED="1508060212822" TEXT="This is an expression">
<node CREATED="1508060217178" ID="ID_756834086" MODIFIED="1508060218134" TEXT="Lam"/>
<node CREATED="1508060218498" ID="ID_1748193413" MODIFIED="1508060219534" TEXT="Pi"/>
<node CREATED="1508060219778" ID="ID_286263278" MODIFIED="1508060220734" TEXT="App"/>
<node CREATED="1508060221322" ID="ID_1866349408" MODIFIED="1508060222558" TEXT="Var"/>
<node CREATED="1508060222786" ID="ID_330463399" MODIFIED="1508060223751" TEXT="Const"/>
</node>
</node>
</node>
</node>
<node CREATED="1508060228611" ID="ID_1396919006" MODIFIED="1508060235918" TEXT="Context">
<node CREATED="1508060237027" ID="ID_1872127063" MODIFIED="1508060242134" TEXT="A view of RawContext"/>
<node CREATED="1508060242970" ID="ID_1922086768" MODIFIED="1508060338608" TEXT="gets built up">
<node CREATED="1508060338594" ID="ID_241259839" MODIFIED="1508060343518" TEXT="data constructors">
<node CREATED="1508060246275" ID="ID_86226174" MODIFIED="1508060328774" TEXT="(empty except return type)">
<node CREATED="1508060293403" ID="ID_330514658" MODIFIED="1508060346198" TEXT="var">
<node CREATED="1508060350938" ID="ID_1801194496" MODIFIED="1508060365783" TEXT="reference to an unknown var specified by name"/>
</node>
<node CREATED="1508060346578" ID="ID_1267004348" MODIFIED="1508060347278" TEXT="const"/>
</node>
<node CREATED="1508060378547" ID="ID_529601992" MODIFIED="1508060380558" TEXT="Lam">
<node CREATED="1508060382827" ID="ID_1427793210" MODIFIED="1508060394119" TEXT="args">
<node CREATED="1508060395138" ID="ID_639861804" MODIFIED="1508060403239" TEXT="varName"/>
<node CREATED="1508060403570" ID="ID_366153605" MODIFIED="1508060415942" TEXT="expr : Context"/>
</node>
</node>
<node CREATED="1508060639194" ID="ID_1749187911" MODIFIED="1508060641990" TEXT="Pi">
<node CREATED="1508060382827" ID="ID_475964381" MODIFIED="1508060394119" TEXT="args">
<node CREATED="1508060395138" ID="ID_727122353" MODIFIED="1508060403239" TEXT="varName"/>
<node CREATED="1508060403570" ID="ID_1427883942" MODIFIED="1508060415942" TEXT="expr : Context"/>
</node>
</node>
<node CREATED="1508060647619" ID="ID_346394483" MODIFIED="1508060651406" TEXT="App">
<node CREATED="1508060653059" ID="ID_245430238" MODIFIED="1508060654134" TEXT="args">
<node CREATED="1508060655858" ID="ID_1225950775" MODIFIED="1508060658454" TEXT="expr1"/>
<node CREATED="1508060658995" ID="ID_911435144" MODIFIED="1508060663590" TEXT="expr2"/>
<node CREATED="1508060666651" ID="ID_1503350762" MODIFIED="1508060728047" TEXT="isApplicable expr1 expr2">
<node CREATED="1508060729075" ID="ID_1370158093" MODIFIED="1508060756934" TEXT="Proof that expr2 can be applied to expr1">
<node CREATED="1508060758114" ID="ID_178988668" MODIFIED="1508060785759" TEXT="expr1 must be lam"/>
<node CREATED="1508060786066" ID="ID_867430341" MODIFIED="1508060794966" TEXT="types must match"/>
</node>
</node>
</node>
</node>
</node>
</node>
</node>
<node CREATED="1508061114682" ID="ID_1465827535" MODIFIED="1508061118646" TEXT="NormalizedContext">
<node CREATED="1508061119578" ID="ID_74843139" MODIFIED="1508061129246" TEXT="Context without Apps, except for Data Apps">
<node CREATED="1508061130226" ID="ID_662687163" MODIFIED="1508061134542" TEXT="Such as Var n x"/>
</node>
</node>
<node CREATED="1508060435923" ID="ID_753433590" MODIFIED="1508060441454" TEXT="DeBrujinContext">
<node CREATED="1508060442610" ID="ID_615097316" MODIFIED="1508060446295" TEXT="???? do we need this"/>
<node CREATED="1508060446562" ID="ID_257827943" MODIFIED="1508060457438" TEXT="vars converted to debrujin indexes"/>
</node>
</node>
</node>
<node CREATED="1508665826439" ID="ID_133812594" MODIFIED="1508665852579" TEXT="10/22/17 - idris modification based AN">
<node CREATED="1508666241999" ID="ID_391710424" MODIFIED="1508666246658" TEXT="System stored as TT">
<node CREATED="1508666251623" ID="ID_228709962" MODIFIED="1508666465354" TEXT="Must be in a partially compiled state. Must be able to easily back out changes and replace">
<node CREATED="1508666468302" ID="ID_367699481" MODIFIED="1508666474938" TEXT="use whiteboard or something similar!"/>
</node>
</node>
<node CREATED="1508665853846" ID="ID_378232147" MODIFIED="1508666011667" TEXT="init">
<node CREATED="1508666015830" ID="ID_236642787" MODIFIED="1508666026827" TEXT="This will be the &quot;rom&quot; which is first to start up"/>
<node CREATED="1508666027871" ID="ID_1343971136" MODIFIED="1508666509866" TEXT="It will setup the network, and user interface and begin reading blocks"/>
</node>
</node>
<node CREATED="1509016654411" ID="ID_1754238691" MODIFIED="1509016819994" TEXT="10/26/17">
<node CREATED="1509016819979" ID="ID_1107323319" MODIFIED="1509019118258" TEXT="plan1">
<node CREATED="1509016661754" ID="ID_356312359" MODIFIED="1509016815733" TEXT="With the elaborator, I can reference individual terms, and view their implementation in the reflected types"/>
<node CREATED="1509016699954" ID="ID_1307065166" MODIFIED="1509016808110" TEXT="Given that, I just need to check that each of these terms are not doing anything bad, and are total"/>
<node CREATED="1509016730834" ID="ID_963956591" MODIFIED="1509016747998" TEXT="Then I can load them directly into Idris"/>
<node CREATED="1509018546567" ID="ID_843947667" MODIFIED="1509018570994" TEXT="Except I can&apos;t run the elaborator within the executable">
<node CREATED="1509018572007" ID="ID_1360648595" MODIFIED="1509018586851" TEXT="The only way to run it seems to be %runElab, which can&apos;t be specified within a function"/>
</node>
<node CREATED="1509016748618" ID="ID_1675340291" MODIFIED="1509016784878" TEXT="Future">
<node CREATED="1509016785858" ID="ID_798804177" MODIFIED="1509016801829" TEXT="I need to be able to unload terms"/>
<node CREATED="1509016826778" ID="ID_1474192503" MODIFIED="1509016837574" TEXT="I need to be able to store these terms in a special place so they can be unloaded">
<node CREATED="1509016838642" ID="ID_1623749189" MODIFIED="1509016883494" TEXT="This will require code modification to IState within the Idris source"/>
<node CREATED="1509016907810" ID="ID_1930553485" MODIFIED="1509016927070" TEXT="Or maybe I can call into Idris using my own special monad? Or modified IState?">
<node CREATED="1509016929322" ID="ID_1168677845" MODIFIED="1509016932501" TEXT="Probably not..."/>
</node>
</node>
</node>
</node>
<node CREATED="1509018996182" ID="ID_1599091546" MODIFIED="1509018998090" TEXT="problems">
<node CREATED="1509018999078" ID="ID_1249833485" MODIFIED="1509019015402" TEXT="We don&apos;t want to interpret the root code, it should be compiled">
<node CREATED="1509019016862" ID="ID_1152972557" MODIFIED="1509019028922" TEXT="Yet it has to interface with the non compiled untrusted code">
<node CREATED="1509019030462" ID="ID_1635141059" MODIFIED="1509019040417" TEXT="We could compile the untrusted code as well">
<node CREATED="1509019044502" ID="ID_491848940" MODIFIED="1509019047234" TEXT="as a future change"/>
</node>
</node>
</node>
</node>
<node CREATED="1509019119253" ID="ID_1484931572" MODIFIED="1509019120169" TEXT="plan2">
<node CREATED="1509019121590" ID="ID_1026619019" MODIFIED="1509019126794" TEXT="Two things we need to work out">
<node CREATED="1509019127838" ID="ID_429218747" MODIFIED="1509019513733" TEXT="A) How to create TT "/>
</node>
</node>
</node>
<node CREATED="1509177503948" ID="ID_753758324" MODIFIED="1509177510624" TEXT="10/28/17">
<node CREATED="1509177511044" ID="ID_213775086" MODIFIED="1509177540544" TEXT="if we create a haskell backend for idris, we no longer need to subvert idris to use it">
<node CREATED="1509177542780" ID="ID_1793260302" MODIFIED="1509177553064" TEXT="Because we could include idris as a library"/>
<node CREATED="1509177553943" ID="ID_1901255280" MODIFIED="1509177738921" TEXT="We&apos;d end up with the same problem though, because we&apos;d need to compile the haskel to run it as a c prog"/>
</node>
<node CREATED="1509235284026" ID="ID_1317676236" MODIFIED="1509235302968" TEXT="potential issues">
<node CREATED="1509235310548" ID="ID_1896099531" MODIFIED="1509235496961" TEXT="network dos attacks">
<node CREATED="1509235499557" ID="ID_1007768082" MODIFIED="1509235521737" TEXT="Do we handle this in the block chain?"/>
<node CREATED="1509235523389" ID="ID_876434817" MODIFIED="1509235546113" TEXT="Or directly in root?">
<node CREATED="1509235549261" ID="ID_1439601999" MODIFIED="1509235613384" TEXT="It would probably be better here. There has to be a contract that a block chain can always be readable. If we allow a block chain the power to fight against DOS attacks, then it could also conceivably prevent new comers from joining"/>
</node>
</node>
<node CREATED="1509237934776" ID="ID_855607170" MODIFIED="1509237951500" TEXT="Genesis + blocks">
<node CREATED="1509237924762" ID="ID_97981098" MODIFIED="1509237927932" TEXT="network access ">
<node CREATED="1509235853380" ID="ID_232245744" MODIFIED="1509236108272" TEXT="Root must allow restricted block to form p2p networks">
<node CREATED="1509235920332" ID="ID_1504962398" MODIFIED="1509235927176" TEXT="What about direct access to internet?">
<node CREATED="1509235940308" ID="ID_612703471" MODIFIED="1509235968920" TEXT="Seems dangerous, since sub contexts could possibly hack other programs on localhost"/>
</node>
</node>
<node CREATED="1509237856841" ID="ID_20169005" MODIFIED="1509237885437" TEXT="I think we should allow sub-contexts to be connected to, but not be able to connect outwards">
<node CREATED="1509237886577" ID="ID_1753239845" MODIFIED="1509237905388" TEXT="You have to invite the devil into your home before he can steal your soul, basically"/>
</node>
</node>
</node>
<node CREATED="1509235723116" ID="ID_1425388587" MODIFIED="1509235725273" TEXT="subcontexts">
<node CREATED="1509235730060" ID="ID_1192201635" MODIFIED="1509237924773" TEXT="How are they created?">
<node CREATED="1509235778541" ID="ID_645673728" MODIFIED="1509236171848" TEXT="Each subcontext will get the ability to make looping threads with state that can create its own seperate p2p network "/>
</node>
<node CREATED="1509235741420" ID="ID_376911340" MODIFIED="1509235744080" TEXT="How are they run?">
<node CREATED="1509236008212" ID="ID_1085505714" MODIFIED="1509236074224" TEXT="As far as creating blocks in the parent, they can call methods in parent context to create blocks (given they have the appropriate winning hash, etc)"/>
</node>
</node>
<node CREATED="1509236255859" ID="ID_1997064537" MODIFIED="1509236332112" TEXT="What about out of memory problems in restricted code?">
<node CREATED="1509236302132" ID="ID_495443057" MODIFIED="1509236316328" TEXT="We need to add extensions to Restricted mode for memory management"/>
</node>
<node CREATED="1509236425867" ID="ID_1141954248" MODIFIED="1509236437671" TEXT="CPU busy loops in restricted code?">
<node CREATED="1509236438611" ID="ID_1070535138" MODIFIED="1509236450744" TEXT="Add GAS limit to restricted code"/>
</node>
<node CREATED="1509236358780" ID="ID_786006589" MODIFIED="1509236366935" TEXT="block confirmations and changes"/>
</node>
</node>
<node CREATED="1509783977964" ID="ID_1274800158" MODIFIED="1509961963960" TEXT="11/4/17">
<node CREATED="1509783984770" ID="ID_211950682" MODIFIED="1509783997957" TEXT="Understanding TT (Idris&apos;s kernel language)">
<node CREATED="1509783998778" ID="ID_426208116" MODIFIED="1509784004966" TEXT="RigCount">
<node CREATED="1509784005867" ID="ID_308984254" MODIFIED="1509784011581" TEXT="This is erasure of types"/>
</node>
<node CREATED="1509784012770" ID="ID_1660384993" MODIFIED="1509784024221" TEXT="Data Types"/>
<node CREATED="1509784024625" ID="ID_555371370" MODIFIED="1509784031845" TEXT="Function type checking"/>
<node CREATED="1509784032169" ID="ID_1315824574" MODIFIED="1509784033606" TEXT="Interface"/>
</node>
<node CREATED="1509790759180" ID="ID_50932295" MODIFIED="1509790956454" TEXT="problems">
<node CREATED="1509790956445" ID="ID_1830284345" MODIFIED="1509790958336" TEXT="loadSource">
<node CREATED="1509790765093" ID="ID_1025573230" MODIFIED="1509790774000" TEXT="loadSource is pretty lax as far as how it does stuff"/>
<node CREATED="1509790774980" ID="ID_1054356211" MODIFIED="1509790782936" TEXT="It handles loading into TT"/>
<node CREATED="1509790783156" ID="ID_49752198" MODIFIED="1509790788408" TEXT="then typechecking"/>
</node>
<node CREATED="1509790959076" ID="ID_720286228" MODIFIED="1509790966015" TEXT="typechecking done by elab">
<node CREATED="1509790967332" ID="ID_1429818665" MODIFIED="1509790972063" TEXT="There is no standard function that does it"/>
<node CREATED="1509790972556" ID="ID_226737972" MODIFIED="1509790986711" TEXT="It seems to be done quite sloppily"/>
<node CREATED="1509790996788" ID="ID_466695879" MODIFIED="1509791009736" TEXT="Nothing that says, ok, this Raw goes in, and we typecheck it"/>
</node>
<node CREATED="1509791019453" ID="ID_1416939890" MODIFIED="1509791022793" TEXT="totality checking">
<node CREATED="1509791023788" ID="ID_584260288" MODIFIED="1509791034096" TEXT="We wait until each mutual is finished to do totality checking"/>
<node CREATED="1509791035380" ID="ID_932763802" MODIFIED="1509791437201" TEXT="It gets checked twice, once for defered items? See loadSource"/>
</node>
<node CREATED="1509791438989" ID="ID_1719638555" MODIFIED="1509791444552" TEXT="proof of Void">
<node CREATED="1509791447013" ID="ID_1904077546" MODIFIED="1509791455800" TEXT="There are two cases that I know about">
<node CREATED="1509791456084" ID="ID_213124703" MODIFIED="1509791458953" TEXT="%reflection case">
<node CREATED="1509791598901" ID="ID_137443430" MODIFIED="1509791601000" TEXT="https://github.com/idris-lang/Idris-dev/issues/2716"/>
</node>
<node CREATED="1509791459212" ID="ID_880387391" MODIFIED="1509791663608" TEXT="injectivity">
<node CREATED="1509791664476" ID="ID_74551749" MODIFIED="1509791665336" TEXT="https://github.com/idris-lang/Idris-dev/issues/3687"/>
</node>
</node>
<node CREATED="1509791673053" ID="ID_1447899562" MODIFIED="1509791676392" TEXT="There are probably more"/>
</node>
</node>
<node CREATED="1509837098298" ID="ID_396347725" MODIFIED="1509837100582" TEXT="ideas">
<node CREATED="1509837101938" ID="ID_1496103855" MODIFIED="1509837206183" TEXT="AN written in Idris">
<node CREATED="1509837206170" ID="ID_987021112" MODIFIED="1509837208926" TEXT="plan">
<node CREATED="1509837110490" ID="ID_187243171" MODIFIED="1509837124253" TEXT="We convert Idris to TT using QuasiQuoting">
<node CREATED="1509837125194" ID="ID_356561214" MODIFIED="1509837142766" TEXT="May need to alter Idris a little to support an entire module being quasi quoted"/>
</node>
<node CREATED="1509837148186" ID="ID_104040757" MODIFIED="1509837181614" TEXT="Translate TT into AN langauge, or use directly"/>
<node CREATED="1509837182066" ID="ID_1372293711" MODIFIED="1509837191167" TEXT="Typecheck and totality check AN"/>
<node CREATED="1509837191697" ID="ID_1205912479" MODIFIED="1509837197023" TEXT="Reverse translation also possible"/>
</node>
<node CREATED="1509837209330" ID="ID_338179054" MODIFIED="1509837244494" TEXT="Core will be written in Idris"/>
<node CREATED="1509837246794" ID="ID_25322095" MODIFIED="1509837252206" TEXT="Will it contain the interpreter?">
<node CREATED="1509837259705" ID="ID_630739344" MODIFIED="1509837434022" TEXT="Otherwise, how to type check TT?">
<node CREATED="1509837265018" ID="ID_355639301" MODIFIED="1509837276102" TEXT="IPC?">
<node CREATED="1509837379034" ID="ID_705692751" MODIFIED="1509837400253" TEXT="We could alter idris to accept TT using IPC from Autonomic">
<node CREATED="1509837401929" ID="ID_1613237639" MODIFIED="1509837411014" TEXT="This would be the IDE of autonomic, basically"/>
</node>
</node>
</node>
</node>
</node>
<node CREATED="1509876417023" ID="ID_1477196274" MODIFIED="1509876423427" TEXT="Examine the ibc creation/reading">
<node CREATED="1509876424886" ID="ID_159381300" MODIFIED="1509876454162" TEXT="An ibc should be in TT, and should have everything I need to know about what Idris needs to run">
<node CREATED="1509876455790" ID="ID_206529927" MODIFIED="1509876461970" TEXT="Typechecking will be a little tricky here, though"/>
</node>
</node>
</node>
</node>
</node>
<node CREATED="1480463573465" FOLDED="true" ID="ID_697925613" MODIFIED="1510032825585" POSITION="right" TEXT="wiki">
<node CREATED="1480510613617" ID="ID_1303643363" MODIFIED="1510031515830" TEXT="backups">
<node CREATED="1480510626508" ID="ID_580202796" MODIFIED="1480510627789" TEXT="old">
<node CREATED="1480463578713" ID="ID_1529077249" MODIFIED="1480463580296" TEXT="backups">
<node CREATED="1480463581203" ID="ID_129524630" MODIFIED="1480463591301" TEXT="schedule mysqldump"/>
<node CREATED="1480463591778" ID="ID_239051966" MODIFIED="1480463603934" TEXT="use rclone to backup the wiki directly">
<node CREATED="1480463605099" ID="ID_965645997" MODIFIED="1480463624304" TEXT="*NOT* the whole web site, so I don&apos;t forget and put something sensitive there"/>
</node>
<node CREATED="1480463884113" ID="ID_937885147" MODIFIED="1480463887737" TEXT="using tim as backup user">
<node CREATED="1480463888687" ID="ID_1768852026" MODIFIED="1480463899754" TEXT="calling a bunch of scripts, such as rclone seems not a good idea"/>
</node>
</node>
<node CREATED="1480464042096" ID="ID_787965294" MODIFIED="1480464043576" TEXT="borg?">
<node CREATED="1480464045908" ID="ID_1209126708" MODIFIED="1480464050332" TEXT="too much space I think"/>
</node>
<node CREATED="1480464180975" ID="ID_268512725" MODIFIED="1480464190743" TEXT="maybe put TIMBACKUP in directories to backup">
<node CREATED="1480464201036" ID="ID_1091771573" MODIFIED="1480464216457" TEXT="if an whitespace only file or empty then backup entire directory tree"/>
<node CREATED="1480464217041" ID="ID_1034710701" MODIFIED="1480464225133" TEXT="otherwise only files mentioned"/>
</node>
<node CREATED="1480464302150" ID="ID_1849024581" MODIFIED="1480464308012" TEXT="how to backup?">
<node CREATED="1480464309135" ID="ID_1709809376" MODIFIED="1480464313083" TEXT="borg configuration?"/>
<node CREATED="1480464324424" ID="ID_1063899343" MODIFIED="1480464329542" TEXT="we need two types">
<node CREATED="1480464330664" ID="ID_428557733" MODIFIED="1480464336542" TEXT="historical">
<node CREATED="1480464373307" ID="ID_1183509358" MODIFIED="1480464378624" TEXT="should some have a timelimit?">
<node CREATED="1480464426157" ID="ID_265654762" MODIFIED="1480464431695" TEXT="borg prune will help with that"/>
<node CREATED="1480464539696" ID="ID_1381815935" MODIFIED="1480464556844" TEXT="we can use -P option to only prune a certain set of backups"/>
</node>
</node>
<node CREATED="1480464336936" ID="ID_1753267247" MODIFIED="1480464341741" TEXT="non-historical">
<node CREATED="1480464342929" ID="ID_1923885421" MODIFIED="1480464347906" TEXT="ie one copy good enough"/>
</node>
</node>
</node>
<node CREATED="1480464577646" ID="ID_1410932471" MODIFIED="1480464599850" TEXT="each directory tree to be backed up contains a TIMBACKUP">
<node CREATED="1480465787011" ID="ID_740088974" MODIFIED="1480465803905" TEXT="it will contain straight borg options.... --exclude &quot;bla bla bla&quot;, etc."/>
</node>
</node>
<node CREATED="1480510628211" ID="ID_1764170676" MODIFIED="1480510637265" TEXT="using borg_helper and rclone to google drive"/>
</node>
<node CREATED="1480510658102" ID="ID_881161739" MODIFIED="1480510658102" TEXT=""/>
</node>
<node CREATED="1488097395576" FOLDED="true" ID="ID_125458100" MODIFIED="1497250331163" POSITION="right" TEXT="irc">
<node CREATED="1488097397447" ID="ID_1962010074" MODIFIED="1488097406997" TEXT="closed phase">
<node CREATED="1488097408455" ID="ID_770734358" MODIFIED="1488097411956" TEXT="type system"/>
<node CREATED="1488097412208" ID="ID_1472381158" MODIFIED="1488097416827" TEXT="chain rules"/>
<node CREATED="1488097418832" ID="ID_1878208747" MODIFIED="1488097419980" TEXT="hooks">
<node CREATED="1488097420823" ID="ID_298417772" MODIFIED="1488097426122" TEXT="blockverify"/>
<node CREATED="1488097426543" ID="ID_451793537" MODIFIED="1488097429067" TEXT="depth calc"/>
<node CREATED="1488097434535" ID="ID_420130497" MODIFIED="1488097435867" TEXT="get work"/>
</node>
<node CREATED="1488097450638" ID="ID_1174389825" MODIFIED="1488097451154" TEXT="irc">
<node CREATED="1488097452014" ID="ID_569654199" MODIFIED="1488097454594" TEXT="02:46 &lt; HMCa&gt; we have our typesystem there, we have our chain rules giving background, we have our blockverify and depth calc and getwork etc hooks... all that fun stuff that we&apos;ve talked on... "/>
<node CREATED="1488097471518" ID="ID_908341383" MODIFIED="1488097480197">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      02:48 &lt; HMCa&gt; mostly, it is coordinating behaviours of open phase, to constrain and define &quot;what playing fair should mean&quot; wrt open phase operation
    </p>
    <p>
      02:49 &lt; HMCa&gt; both so that our node &quot;plays fair&quot; (when/asmuchas we want it to)... and so that we know how to observe others' fairnesses
    </p>
    <p>
      
    </p>
    <p>
      07:19 &lt; stoopkid&gt; so, block comes in, closed phase reasoning is started, and the first thing it does is find the chain-tip that this block is associated with, if it has it, takes that chain-tip, steps into that context, and runs the block-validity check on that block, and if it passes then the block is added to that chain, opened up, and the rule-changes are extracted and
    </p>
    <p>
      07:19 &lt; stoopkid&gt; applied to that context, which you'd store essentially as the current &quot;ledger&quot; for that chain
    </p>
    <p>
      07:34 &lt;@HMCa&gt; sounds about right
    </p>
    <p>
      07:36 &lt; stoopkid&gt; so after closed phase is done with this it tells the open-phase to ask peers for more blocks?
    </p>
  </body>
</html></richcontent>
</node>
</node>
</node>
<node CREATED="1497046519142" ID="ID_200590760" MODIFIED="1497046687287" TEXT="core">
<node CREATED="1497046687255" ID="ID_1611638491" MODIFIED="1497046704921" TEXT="the reference client">
<node CREATED="1497046526782" ID="ID_1495078402" MODIFIED="1497046543632">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      16:33 &lt; HMC_a&gt; ok, so we get 3+ &quot;obviously publicly different&quot; people who stand up together and announce they're goijg to agree to attempt to agree.&#160;&#160;Now they must proceed with &quot;agreeing how to agree&quot;... meaning they muat decide upon a framework
    </p>
    <p>
      16:33 &lt; HMC_a&gt; call this &quot;metagame&quot;
    </p>
    <p>
      16:33 &lt; HMC_a&gt; this is where we are now, today
    </p>
    <p>
      16:34 &lt; pager&gt; this is genesis\metagame\seeding?
    </p>
    <p>
      16:34 &lt; HMC_a&gt; in a pre-game metagame, of sorts, agreeing to a logical framework of agreements.&#160;&#160;Call this framework &quot;core&quot; or &quot;reference client&quot; if you will
    </p>
  </body>
</html></richcontent>
</node>
</node>
<node CREATED="1497046705446" ID="ID_1430226389" MODIFIED="1497047228490" TEXT="must be completely fair">
<node CREATED="1497046830901" ID="ID_1325959288" MODIFIED="1497046852751">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      16:40 &lt; HMC_a&gt; noone should own it, noone should profit (pre-genesis) directly from core having come into being
    </p>
    <p>
      16:41 &lt; HMC_a&gt; noone should be able to imply ulterior motives of the agreement
    </p>
    <p>
      16:41 &lt; HMC_a&gt; it has to be, in a sense, the most undeniably fair, open-ended contract
    </p>
    <p>
      16:41 &lt; pager&gt; can there be a agreement that the real agreement can only be made post-genesis?
    </p>
    <p>
      16:41 &lt; HMC_a&gt; (as much as we can manage, being humans and all, anyway)
    </p>
  </body>
</html></richcontent>
</node>
</node>
</node>
<node CREATED="1497046929469" ID="ID_389914226" MODIFIED="1497046931785" TEXT="genesis">
<node CREATED="1497046933709" ID="ID_1181189958" MODIFIED="1497046943809" TEXT="the code that runs ontop of core">
<node CREATED="1497046946749" ID="ID_340502692" MODIFIED="1497047124378">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      16:44 &lt; HMC_a&gt; we get to &quot;writing genesis&quot; which means basically building this whole &quot;second&quot; thing, the initial rules of the initial chain (&quot;root&quot;)
    </p>
    <p>
      16:45 &lt; HMC_a&gt; this can be an entirely other set of participants who agree to agree to the framework of agreement set out as &quot;core&quot; ;-)
    </p>
    <p>
      16:45 &lt; HMC_a&gt; more fun implciions here
    </p>
    <p>
      16:45 &lt; HMC_a&gt; and an open question or two, like: &quot;most genesis rules define 'roster'?&quot;
    </p>
    <p>
      16:45 &lt; HMC_a&gt; *must
    </p>
    <p>
      16:46 &lt; HMC_a&gt; me, I think probably yes
    </p>
    <p>
      16:46&#160;&#160;* HMC_a shrugs
    </p>
  </body>
</html></richcontent>
</node>
</node>
<node CREATED="1497047195398" ID="ID_932328051" MODIFIED="1497047221705" TEXT="along with core, must be &quot;unbiased&quot;">
<node CREATED="1497047212014" ID="ID_576583106" MODIFIED="1497047215888">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      16:47 &lt; HMC_a&gt; which this rule set defines...
    </p>
    <p>
      16:48 &lt; HMC_a&gt; all of the rest of the security ans *stability* of the network is predicated upon this chain's security and stability, directly or indirectly
    </p>
    <p>
      16:49 &lt; HMC_a&gt; if it breaks/stalls, the network *will* fragment in the interim, and some assurances go out of the window
    </p>
    <p>
      16:50 &lt; pager&gt; i believe the system has a hierarchical structure?
    </p>
    <p>
      16:50 &lt; HMC_a&gt; so this chain is pretty critical... best it entail as *little* as possible, and similarly to core best it be &quot;obviously correct and unbiased&quot; (as much as is possible for us mere humans anyway)
    </p>
    <p>
      16:51 &lt; HMC_a&gt; yes, hierachichal, which I'm coming too...
    </p>
  </body>
</html></richcontent>
</node>
</node>
</node>
<node CREATED="1488097489494" ID="ID_458468918" MODIFIED="1488097496851" TEXT="object language runtime"/>
<node CREATED="1488097497311" ID="ID_1853672656" MODIFIED="1488347903798" TEXT="open phase">
<node CREATED="1488245523936" ID="ID_1144948318" MODIFIED="1488245536352">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      21:43 &lt; HMCa&gt; imagine python and idris calling eachother recursively.... here our python is the untyped preds and our idris is the typed preds
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1488245922998" ID="ID_997281409" MODIFIED="1488245930589">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      05:57 &lt; HMCa_&gt; &quot;The synergy Tau offers to Ethereum is that the critical portions of a smart contract can be written in Tau but the less critical portion can be written in Solidity&quot;
    </p>
    <p>
      05:59 &lt; HMCa_&gt; tau offers this sort of &quot;less critical mode&quot; operation... and not using it makes the &quot;critical mode&quot; operation of questionable value!
    </p>
    <p>
      06:00 &lt; HMCa_&gt; tau should handle both sets of concerns in coordination, ofc
    </p>
    <p>
      06:01 &lt; HMCa_&gt; this is exactly why we have &quot;open phase&quot; reasoning
    </p>
    <p>
      06:02 &lt; HMCa_&gt; for all that un-critical (but still secure-by-vm-isolation) sideband &quot;stuff&quot; that should happen faster by way of less assurances (like trace proofs ofc)
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1488347903781" ID="ID_1670888239" MODIFIED="1488347918047" TEXT="is open phase a way to use different types of logics?">
<node CREATED="1488347879763" ID="ID_1458102119" MODIFIED="1488347901942">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      0828080735&lt;stoopkid&gt; so, when i look around at different type theory papers, i see different systems of type theory, i.e. different sets of natural deduction rules, or some systems with extra rules/axioms added, etc..
    </p>
    <p>
      0828080752&lt;stoopkid&gt; so, is that essentially what the user is building?
    </p>
    <p>
      0828080812&lt;stoopkid&gt; like, let's say we want to define the rules of our nomic game
    </p>
    <p>
      0828080829&lt;_HMC_&gt; well, tricky question....
    </p>
    <p>
      0828080908&lt;_HMC_&gt; they &quot;can&quot; do this in the open phase, but in the closed phase they'd have to extend the deductive rules of the typesys
    </p>
  </body>
</html></richcontent>
</node>
</node>
<node CREATED="1488348161107" ID="ID_627374966" MODIFIED="1488348172479" TEXT="rules are a calling convention. They are not represented in MLTT">
<node CREATED="1488348174361" ID="ID_1495034401" MODIFIED="1488348177977">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      0828081710&lt;stoopkid&gt; ok scratch that, what i'm really concerned with here is user-defined n3 rules that aren't &quot;part of&quot; MLTT, and how those fit into our framework
    </p>
    <p>
      0828081728&lt;_HMC_&gt; and yes, the contexts are each &quot;self contained&quot; theories extending the typesystem and built-in (partial) theories
    </p>
    <p>
      0828081831&lt;_HMC_&gt; ans yes, necessarily consistent, meaning they typecheck with (and must &quot;complete&quot;) those partial theories
    </p>
    <p>
      0828081857&lt;stoopkid&gt; like, {} =&gt; {&quot;John&quot; a Person} is &quot;part of&quot; MLTT because it's using the typing judgement from MLTT, but, {} =&gt; {&quot;John&quot; plays Nomics} isn't really &quot;part of&quot; MLTT, unless you consider &quot;plays&quot; to be a function
    </p>
    <p>
      0828081920&lt;stoopkid&gt; function/functor
    </p>
    <p>
      0828081938&lt;_HMC_&gt; sure, the latter &quot;alone&quot; is considered meaningless in any closed phase reasoning
    </p>
    <p>
      0828082010&lt;stoopkid&gt; so you'd have to give a type to the 'plays' predicate
    </p>
    <p>
      0828082030&lt;_HMC_&gt; &quot;plays&quot; must have some ontological meaning defined, yes
    </p>
    <p>
      0828082041&lt;stoopkid&gt; like, plays rdf:domain Person. plays rdf:range Game.
    </p>
    <p>
      0828082103&lt;_HMC_&gt; at least ;-)
    </p>
    <p>
      0828082129&lt;stoopkid&gt; ok so, rules are functions, functions are rules, essentially?
    </p>
    <p>
      0828082226&lt;_HMC_&gt; in a sense.&#160;&#160;More like rules are a &quot;calling covention&quot;...
    </p>
    <p>
      0828082235&lt;_HMC_&gt; *convention
    </p>
  </body>
</html></richcontent>
</node>
</node>
</node>
<node CREATED="1488097500928" ID="ID_1934903760" MODIFIED="1488097505018" TEXT="builtins"/>
<node CREATED="1488097549278" ID="ID_236303155" MODIFIED="1488097554569" TEXT="extralogical theory">
<node CREATED="1488244182383" ID="ID_296602419" MODIFIED="1488244197146" TEXT="communicates with kb on behalf of open phase code"/>
</node>
<node CREATED="1488097557277" ID="ID_146348503" MODIFIED="1488097560370" TEXT="knowledge base"/>
<node CREATED="1488097560687" ID="ID_1636421699" MODIFIED="1488097567194" TEXT="naive tauchain">
<node CREATED="1488097568053" ID="ID_759054078" MODIFIED="1488097568649" TEXT="irc">
<node CREATED="1488097571103" ID="ID_109432583" MODIFIED="1488097612482">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      19:51 &lt; HMC_A&gt; we call this thought experiment &quot;the naive tauchain&quot; :-D
    </p>
    <p>
      19:52 &lt; HMC_A&gt; because it illustrates precisely how *not* to build tauchain to succeed
    </p>
    <p>
      19:52 &lt; HMC_A&gt; (and it is no coincidence that it is awfully ethereum-like in construction)
    </p>
    <p>
      19:52 &lt; HMC_A&gt; ok, so it goes something like this:
    </p>
    <p>
      19:53 &lt; HMC_A&gt; imagine taking bitcoin and removing all of the economic logic from it... so, no coin, no account balances, no reward, no inputs/outputs, etc...
    </p>
    <p>
      19:53 &lt; HMC_A&gt; but we otherwise leave the block structure and tx structure in tact
    </p>
    <p>
      19:53 &lt; HMC_A&gt; specifically, we have transactions that are only scripts.
    </p>
    <p>
      19:53 &lt; HMC_A&gt; now, let's say we replace the limited stack based language of btc scripts with something like, i dunno...
    </p>
    <p>
      19:53 &lt; HMC_A&gt; python
    </p>
    <p>
      19:53 &lt; HMC_A&gt; or javascript
    </p>
    <p>
      19:54 &lt; HMC_A&gt; or &quot;insert your favorite turing complete procedural language here&quot;
    </p>
    <p>
      19:54 &lt; HMC_A&gt; now, let's say that instead of evaluating each script in isolation, we keep a single vm instance of our language running to do all evaluations, so definitions persist
    </p>
    <p>
      19:55 &lt; HMC_A&gt; ok, now, finally, we will redefine block validity in an odd way... we will have our client decide block validity at any given time by passing a candidate block into a specially named function in this script vm
    </p>
    <p>
      19:55 &lt; HMC_A&gt; and let's say we put in our genesis block some script that defines the initial block validity condition to be just like btc hashing rounds, to give us something to start with.
    </p>
    <p>
      19:56 &lt; HMC_A&gt; ok, so we've taken bitcoin, stripped it of the coin, extended its scripting to be general purpose, and turned it into a nomic (made it self-describing block-to-block)
    </p>
    <p>
      19:56 &lt; HMC_A&gt; with me so far?
    </p>
  </body>
</html></richcontent>
</node>
</node>
</node>
<node CREATED="1488097505679" ID="ID_236110408" MODIFIED="1488245510978" TEXT="overview">
<node CREATED="1488097513343" ID="ID_1462257050" MODIFIED="1488097517647">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      03:15 &lt; stoopkid&gt; what else does &quot;the whole language&quot; consist of besides &quot;the mltt parts&quot; as concerns &quot;the wire protocol&quot; ?
    </p>
    <p>
      03:16 &lt; HMCa&gt; lots of things... the object language runtime, the open phase language, the builtins definitions, etc
    </p>
    <p>
      <br />
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1488097622333" ID="ID_831012618" MODIFIED="1488097624368" TEXT="01:51 &lt; stoopkid&gt; ok so yea, i imagine verified code repo&apos;s from the context, transformations to code and data (including the crowdsourcing) auth&apos;d by some means to get on chain, and the rest local, downloaded into knowledge-base and run on tau-client "/>
<node CREATED="1488098964734" ID="ID_1381744824" MODIFIED="1488244174385" TEXT="hmc diagram overview">
<node CREATED="1488098967374" ID="ID_1775999040" MODIFIED="1488098969738" TEXT="http://imgur.com/0EL0MwK"/>
</node>
<node CREATED="1488347685556" ID="ID_276853797" MODIFIED="1488347685556" TEXT=""/>
</node>
<node CREATED="1488347696179" ID="ID_1486869950" MODIFIED="1488347698680" TEXT="3 languages">
<node CREATED="1488347699788" ID="ID_1156905560" MODIFIED="1488347707393">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      0828080223&lt;stoopkid&gt; ok well, there's essentially 3 different languages being used here:
    </p>
    <p>
      0828080247&lt;stoopkid&gt; lambda calculus, the types language, and natural deduction
    </p>
    <p>
      0828080259&lt;_HMC_&gt; well
    </p>
    <p>
      0828080308&lt;_HMC_&gt; s/lambda/functor/... ;-)
    </p>
    <p>
      0828080318&lt;_HMC_&gt; but sure
    </p>
    <p>
      0828080359&lt;_HMC_&gt; meaning we have &quot;some&quot; reduction calculus defined in terms of our deduction system (and sometimes also types)
    </p>
    <p>
      0828080402&lt;_HMC_&gt; follow?
    </p>
    <p>
      0828080403&lt;stoopkid&gt; lambda/functor sure, we'll get into that in more detail later, but, what you had been showing me up until we started taking a bit of a break was essentially how to map the functors and types languages into the natural dedication language
    </p>
    <p>
      0828080408&lt;stoopkid&gt; deduction* language
    </p>
    <p>
      0828080417* _HMC_ nods
    </p>
    <p>
      0828080524&lt;stoopkid&gt; so, i've seen how natural deduction can run type-checking/inferencing, theorem-proving, and function/functor/lambda execution
    </p>
    <p>
      0828080607&lt;stoopkid&gt; we'll still have to run through the details of that, but my question now is wrt &quot;rules&quot;
    </p>
    <p>
      0828080653&lt;_HMC_&gt; ok
    </p>
  </body>
</html></richcontent>
</node>
</node>
<node CREATED="1497045196323" ID="ID_1625756874" MODIFIED="1497045198033" TEXT="contexts">
<node CREATED="1497045199483" ID="ID_525755586" MODIFIED="1497045607790">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      16:14 &lt; HMC_a&gt; the construct is something like a &quot;gauntlet for theories&quot;... contextualized
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;knowledge spaces *compete* for contribution of participation and resource...
    </p>
    <p>
      16:15 &lt; HMC_a&gt; the network as a whole is something like an ecosystem for &quot;active&quot; knowledge
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;to evolve in
    </p>
    <p>
      16:16 &lt; HMC_a&gt; literally evolve, as in what contexts are weak will die and end, while strong
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;and adaptable contexts can be virtually immortal
    </p>
    <p>
      16:16 &lt; pager&gt; yes
    </p>
    <p>
      16:19 &lt; HMC_a&gt; every context is responsible for its' own internal security, while also
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;depending on &quot;parent&quot; contexts (which they can contribute resource to) for
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;security in interaction with other contexts
    </p>
    <p>
      16:19 &lt; HMC_a&gt; so
    </p>
    <p>
      16:19 &lt; HMC_a&gt; this makes for an interesting &quot;balancing act&quot; job for the participants
    </p>
    <p>
      16:20 &lt; HMC_a&gt; I like to say that mining on our chains will be a significantly more involved
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;process than usual
    </p>
    <p>
      16:20 &lt; HMC_a&gt; as participants will need to give consideration as to how they feel they would
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;be best served to divvy up their cobtributions within the context tree
    </p>
    <p>
      16:21 &lt; HMC_a&gt; yes, how to weight contexts they particpate in for each resource they
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;contribute
    </p>
  </body>
</html></richcontent>
</node>
</node>
</node>
<node CREATED="1497250335889" FOLDED="true" ID="ID_1634148210" MODIFIED="1509940394863" POSITION="right" TEXT="simple-easier">
<node CREATED="1497252602939" ID="ID_1025713648" MODIFIED="1497252604901" TEXT="internals">
<node CREATED="1497252388111" ID="ID_1044442749" MODIFIED="1497252391723" TEXT="Let">
<node CREATED="1497252392831" ID="ID_1009338128" MODIFIED="1497252600598" TEXT="Let can be represented by App (Lam ...)">
<node CREATED="1497252420129" ID="ID_1913801130" MODIFIED="1497252564787">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      expandLet :: Sym -&gt; Type -&gt; Expr -&gt; Expr -&gt; Expr
    </p>
    <p>
      expandLet i t e b = App (Lam i t b) e
    </p>
  </body>
</html></richcontent>
</node>
</node>
</node>
<node CREATED="1497252658171" ID="ID_511376034" MODIFIED="1497252660767" TEXT="ex">
<node CREATED="1497252669059" ID="ID_374072019" MODIFIED="1497252678667">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      id :: forall (a::*) . a -&gt; a;
    </p>
    <p>
      id a x = x;
    </p>
  </body>
</html></richcontent>
<node CREATED="1497252689308" ID="ID_1870006045" MODIFIED="1497253446484">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      let id :: forall (a :: *) . a-&gt;a = \ (a :: *) (x :: a) -&gt; x
    </p>
    <p>
      in&#160;&#160;id :: forall (a :: *) . a-&gt;a
    </p>
    <p>
      Let &quot;id&quot;
    </p>
    <p>
      &#160;&#160;&#160;&#160;(Pi &quot;a&quot; (Kind Star) (Pi &quot;_&quot; (Var &quot;a&quot;) (Var &quot;a&quot;))) -- type
    </p>
    <p>
      &#160;&#160;&#160;&#160;(Lam &quot;a&quot; (Kind Star) (Lam &quot;x&quot; (Var &quot;a&quot;) (Var &quot;x&quot;))) -- value for &quot;id&quot;
    </p>
    <p>
      &#160;&#160;&#160;&#160;(Var &quot;id&quot;) :: Pi &quot;a&quot; (Kind Star) (Pi &quot;x&quot; (Var &quot;a&quot;) (Var &quot;a&quot;)) -- expression to run within scope of &quot;id&quot;
    </p>
  </body>
</html></richcontent>
</node>
</node>
</node>
</node>
</node>
<node CREATED="1507201214087" FOLDED="true" ID="ID_169974140" MODIFIED="1510032823753" POSITION="right" TEXT="Idris implementation">
<node CREATED="1508831169430" ID="ID_1910893993" MODIFIED="1508831172626" TEXT="definitions">
<node CREATED="1508831173478" ID="ID_1370813647" MODIFIED="1508831174746" TEXT="HOAS">
<node CREATED="1508831175494" ID="ID_885601585" MODIFIED="1508831181297" TEXT="Higher Order Abstract Syntax"/>
</node>
<node CREATED="1508831312646" ID="ID_1913907204" MODIFIED="1509003522463" TEXT="RigCount/RigW">
<hook NAME="accessories/plugins/ClonePlugin.properties">
<Parameters CLONE_ID="CLONE_838290886" CLONE_IDS="ID_1913907204,ID_1285996650," CLONE_ITSELF="true"/>
</hook>
<node CREATED="1508831316629" ID="ID_983686636" MODIFIED="1508831320250" TEXT="from Typecheck.hs">
<node CREATED="1508831321029" ID="ID_1905083900" MODIFIED="1508831326286">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      &#160;&#160;chk :: RigCount -&gt; -- 'sigma' in Bob Atkey's QTT paper, except that
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- for implementation purposes it could be 0, 1 or omega when
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- checking variable usage.
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1508831377981" ID="ID_28008192" MODIFIED="1508831379673" TEXT="https://bentnib.org/quantitative-type-theory.pdf"/>
<node CREATED="1509452007601" ID="ID_1103386406" MODIFIED="1509452010829" TEXT="http://idris.readthedocs.io/en/latest/reference/erasure.html"/>
</node>
<node CREATED="1509003559375" ID="ID_1887581025" MODIFIED="1509003570658" TEXT="Used to determine when we can erase the type from the executable">
<node CREATED="1509003572006" ID="ID_1188770401" MODIFIED="1509003598451" TEXT="To prevent things like a vector containing a nat from holding (S S S S S z) for its length variable">
<node CREATED="1509003620726" ID="ID_1197705412" MODIFIED="1509003625483" TEXT="A lot of additional memory"/>
</node>
<node CREATED="1509003598695" ID="ID_185634086" MODIFIED="1509003616785" TEXT="Instead, length can be erased"/>
</node>
</node>
</node>
<node CREATED="1508828199420" ID="ID_1545095524" MODIFIED="1508828202423" TEXT="Idris">
<node CREATED="1508828203372" ID="ID_1845339777" MODIFIED="1508828207534" TEXT="main monad for REPL"/>
</node>
<node CREATED="1508888340916" ID="ID_897242180" MODIFIED="1508888342320" TEXT="Parser">
<node CREATED="1508888343268" ID="ID_809505522" MODIFIED="1509788301783" TEXT="The parser creates a list of [PDecl] and then uses the elaborator to convert this into IState"/>
<node CREATED="1509788303084" ID="ID_1493882315" MODIFIED="1509788376912" TEXT="loadSource">
<node CREATED="1509788376891" ID="ID_649277500" MODIFIED="1509788379304" TEXT="handes imports">
<node CREATED="1509788330572" ID="ID_193807710" MODIFIED="1509788332992" TEXT="calls">
<node CREATED="1509788336843" ID="ID_844838531" MODIFIED="1509788341423" TEXT="parseImports"/>
<node CREATED="1509788341723" ID="ID_1953201977" MODIFIED="1509788349456" TEXT="getAutoImports"/>
</node>
<node CREATED="1509788381140" ID="ID_1738314768" MODIFIED="1509788392543" TEXT="I believe this writes imports into context"/>
<node CREATED="1509788392819" ID="ID_1244904909" MODIFIED="1509788400471" TEXT="as well as module aliases??"/>
</node>
<node CREATED="1509788401675" ID="ID_1064147541" MODIFIED="1509788436111" TEXT="reads into [PDecl]">
<node CREATED="1509788437876" ID="ID_1231637493" MODIFIED="1509788442463" TEXT="parseProg">
<node CREATED="1509788443299" ID="ID_1218822152" MODIFIED="1509788452462" TEXT="this does the actual conversion into PDecl types"/>
</node>
</node>
<node CREATED="1509789180300" ID="ID_708872630" MODIFIED="1509789193087" TEXT="type checks">
<node CREATED="1509789193579" ID="ID_1824337003" MODIFIED="1509789196439" TEXT="calls">
<node CREATED="1509789197012" ID="ID_1009577302" MODIFIED="1509789200575" TEXT="elabDecls">
<node CREATED="1509789215228" ID="ID_1347968310" MODIFIED="1509789222351" TEXT="This reads in PDecls and updates IState"/>
<node CREATED="1509789330059" ID="ID_719735844" MODIFIED="1509789385408" TEXT="It type checks, but not totality checks"/>
<node CREATED="1509789910244" ID="ID_1143893037" MODIFIED="1509789919984" TEXT="Since it stuffs everything into state, it returns nothing"/>
</node>
</node>
<node CREATED="1509789607724" ID="ID_74065745" MODIFIED="1509789617768" TEXT="totality checking">
<node CREATED="1509789618692" ID="ID_64582814" MODIFIED="1509789647164">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;iReport 3 $ &quot;Totality checking &quot; ++ f
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;logLvl 1 $ &quot;Totality checking &quot; ++ f
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;i &lt;- getIState
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;mapM_ buildSCG (idris_totcheck i)
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;mapM_ checkDeclTotality (idris_totcheck i)
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;mapM_ verifyTotality (idris_totcheck i)
    </p>
    <p>
      
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- Redo totality check for deferred names
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;let deftots = idris_defertotcheck i
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;logLvl 2 $ &quot;Totality checking &quot; ++ show deftots
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;[...snip...]
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;mapM_ buildSCG deftots
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;mapM_ checkDeclTotality deftots
    </p>
  </body>
</html></richcontent>
</node>
</node>
</node>
</node>
<node CREATED="1509788572779" ID="ID_883406285" MODIFIED="1509788575551" TEXT="PDecl type">
<node CREATED="1509788576540" ID="ID_1047006348" MODIFIED="1509788596328" TEXT="Is one to one with idris code... does no reference checking"/>
<node CREATED="1509788596595" ID="ID_558330515" MODIFIED="1509788606632" TEXT="contains PTerm">
<node CREATED="1509788607499" ID="ID_585670582" MODIFIED="1509788610087" TEXT="PTerm has">
<node CREATED="1509788610867" ID="ID_88189456" MODIFIED="1509788623263" TEXT="PQuote Raw">
<node CREATED="1509788624171" ID="ID_408970219" MODIFIED="1509788646847" TEXT="I don&apos;t see this being used within the Parser, but it looks like it corresponds to input used in TT"/>
</node>
</node>
</node>
</node>
</node>
<node CREATED="1509949099915" FOLDED="true" ID="ID_1072423780" MODIFIED="1509961140743" TEXT="Elaborator (outside core)">
<node CREATED="1509949117755" ID="ID_403000485" MODIFIED="1509949600367" TEXT="elabType">
<node CREATED="1509949601195" ID="ID_155465564" MODIFIED="1509949604975" TEXT="called by elabDecl"/>
<node CREATED="1509949605707" ID="ID_1290331246" MODIFIED="1509949632103" TEXT="calls buildType">
<node CREATED="1509950726891" ID="ID_554309781" MODIFIED="1509950760951" TEXT="calls &quot;build&quot; within some monad structure???">
<node CREATED="1509950783596" ID="ID_949535640" MODIFIED="1509950785183" TEXT="build">
<node CREATED="1509950786371" ID="ID_1020987308" MODIFIED="1509950794450">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      -- | Using the elaborator, convert a term in raw syntax to a fully
    </p>
    <p>
      -- elaborated, typechecked term.
    </p>
    <p>
      --
    </p>
    <p>
      -- If building a pattern match, we convert undeclared variables from
    </p>
    <p>
      -- holes to pattern bindings.
    </p>
    <p>
      --
    </p>
    <p>
      -- Also find deferred names in the term and their types
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1509950797587" ID="ID_1290395855" MODIFIED="1509950824495" TEXT="Why does it say it converts Raw syntax to a typedcheck term, when it accepts a PType????"/>
<node CREATED="1509950825851" ID="ID_1779714309" MODIFIED="1509951198688" TEXT="calls the elaborator which produces the TT, but I&apos;m not seeing whether it&apos;s converting the original PDecl to something else, or using it directly. It uses some special monad"/>
</node>
</node>
<node CREATED="1509949632995" ID="ID_1770822900" MODIFIED="1509950358815" TEXT="eventually calls recheck in Typecheck (inside core) which calls Check">
<node CREATED="1509950729187" ID="ID_264296524" MODIFIED="1509950735071" TEXT="This redoes typechecking, again???"/>
</node>
</node>
</node>
</node>
<node CREATED="1508828226156" ID="ID_1916495097" MODIFIED="1508828233887" TEXT="AbsSyntaxTree module">
<node CREATED="1507201219660" ID="ID_1549952069" MODIFIED="1508828236599" TEXT="IState">
<node CREATED="1507201229956" ID="ID_1308936314" MODIFIED="1508828191192" TEXT="Used to store state for the REPL"/>
<node CREATED="1508828332130" ID="ID_1601066336" MODIFIED="1508828340343" TEXT="This is a huge structure">
<node CREATED="1508828341283" ID="ID_1733158184" MODIFIED="1508828355592" TEXT="Not sure how we&apos;re going to implement AN underneath this"/>
</node>
<node CREATED="1508830046615" ID="ID_883526180" MODIFIED="1508830048548" TEXT="tt_ctxt">
<node CREATED="1508829992320" ID="ID_1324864473" MODIFIED="1508830026884" TEXT="This holds the entirety of all the idris code for the current state"/>
<node CREATED="1508830058464" ID="ID_1244975902" MODIFIED="1508830097787" TEXT="The rest of the fields of IState (besides tt_ctxt) just reference this using a Ctxt which is a map of name to name along with additional info"/>
</node>
</node>
<node CREATED="1508828236875" ID="ID_1048977119" MODIFIED="1508828243599" TEXT="idrisInit">
<node CREATED="1508828244852" ID="ID_502465206" MODIFIED="1508828254855" TEXT="initial state for compiler"/>
</node>
<node CREATED="1509791232532" ID="ID_1763992911" MODIFIED="1509791235087" TEXT="PDecl">
<node CREATED="1509791236012" ID="ID_1772683398" MODIFIED="1509791259784" TEXT="Corresponds to the actual AST nodes">
<node CREATED="1509791260588" ID="ID_30713978" MODIFIED="1509791264465" TEXT="Idris source is translated to this"/>
</node>
</node>
</node>
<node CREATED="1508828583202" ID="ID_1580278464" MODIFIED="1508828586023" TEXT="Evaluate module">
<node CREATED="1508828606354" ID="ID_281732491" MODIFIED="1508828608038" TEXT="Context">
<node CREATED="1508828614498" ID="ID_541308663" MODIFIED="1508828616846" TEXT="used by IState">
<node CREATED="1508829977200" ID="ID_1910457114" MODIFIED="1508829986204" TEXT="tt_ctxt"/>
</node>
<node CREATED="1508828617602" ID="ID_1300885363" MODIFIED="1508828652286" TEXT="(from IState)">
<node CREATED="1508828653306" ID="ID_479977012" MODIFIED="1508828653910" TEXT="-- ^ All the currently defined names and their terms "/>
</node>
</node>
<node CREATED="1508828586946" ID="ID_1740486242" MODIFIED="1508828590926" TEXT="TTDecl">
<node CREATED="1508828592083" ID="ID_119408890" MODIFIED="1508828593702" TEXT="tupl of">
<node CREATED="1508828594930" ID="ID_623218843" MODIFIED="1508828602582" TEXT="(Def, RigCount, Injectivity, Accessibility, Totality, MetaInformation)"/>
</node>
<node CREATED="1508828747273" ID="ID_873228978" MODIFIED="1508829883652" TEXT="Contains a reference to the actual TT type or term"/>
</node>
<node CREATED="1508828730810" ID="ID_348711989" MODIFIED="1508828731510" TEXT="Def">
<node CREATED="1508828732386" ID="ID_932938649" MODIFIED="1508828735553">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      {-| A definition is either a simple function (just an expression with a type),
    </p>
    <p>
      &#160;&#160;&#160;a constant, which could be a data or type constructor, an axiom or as an
    </p>
    <p>
      &#160;&#160;&#160;yet undefined function, or an Operator.
    </p>
    <p>
      &#160;&#160;&#160;An Operator is a function which explains how to reduce.
    </p>
    <p>
      &#160;&#160;&#160;A CaseOp is a function defined by a simple case tree -}
    </p>
  </body>
</html></richcontent>
</node>
</node>
</node>
<node CREATED="1508828209045" ID="ID_1805414163" MODIFIED="1510030883990" TEXT="TT">
<node CREATED="1508828477017" ID="ID_1661796219" MODIFIED="1508828916982" TEXT="Ctxt v">
<node CREATED="1509001172218" ID="ID_344418121" MODIFIED="1509001189766" TEXT="I think this corresponds to what you can reference in the file based on a name"/>
<node CREATED="1508828479323" ID="ID_1410454323" MODIFIED="1508981617755">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      A map of names to collections of names
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1508828510690" ID="ID_104919057" MODIFIED="1508828530779">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      -- |Contexts allow us to map names to things. A root name maps to a collection
    </p>
    <p>
      -- of things in different namespaces with that name.
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1508828800418" ID="ID_786274430" MODIFIED="1508828949102" TEXT="v = additional information associate with relationship name"/>
<node CREATED="1508828802946" ID="ID_1480957618" MODIFIED="1508828954782" TEXT="Note that &quot;v&quot; varies">
<node CREATED="1508828832938" ID="ID_1115174293" MODIFIED="1508828853662" TEXT="IState uses a Ctxt with different types in different fields">
<node CREATED="1508828854714" ID="ID_807603350" MODIFIED="1508828878810">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      ex.&#160;&#160;, idris_implicits&#160;&#160;&#160;&#160;:: Ctxt [PArg]
    </p>
    <p>
      &#160;&#160;, idris_statics&#160;&#160;&#160;&#160;&#160;&#160;:: Ctxt [Bool]
    </p>
    <p>
      &#160;&#160;, idris_interfaces&#160;&#160;&#160;:: Ctxt InterfaceInfo
    </p>
  </body>
</html></richcontent>
</node>
</node>
</node>
<node CREATED="1508828965785" ID="ID_173134872" MODIFIED="1508828977749" TEXT="type Ctxt a = Map.Map Name (Map.Map Name a) ">
<node CREATED="1508828982177" ID="ID_1650198286" MODIFIED="1508828985909" TEXT="Name is TT.Name"/>
</node>
</node>
<node CREATED="1508828987681" FOLDED="true" ID="ID_1976454607" MODIFIED="1509006708610" TEXT="Name">
<node CREATED="1508828989698" ID="ID_386130944" MODIFIED="1508829019434">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      -- | Names are hierarchies of strings, describing scope (so no danger of
    </p>
    <p>
      -- duplicate names, but need to be careful on lookup).
    </p>
    <p>
      data Name = UN !T.Text -- ^ User-provided name
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| NS !Name [T.Text] -- ^ Root, namespaces
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| MN !Int !T.Text -- ^ Machine chosen names
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| SN !SpecialName -- ^ Decorated function names
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| SymRef Int -- ^ Reference to IBC file symbol table (used during serialisation)
    </p>
    <p>
      &#160;&#160;deriving (Eq, Ord, Data, Generic, Typeable)
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1508981956426" ID="ID_1947641629" MODIFIED="1508981964998" TEXT="NS">
<node CREATED="1508981966011" ID="ID_639732636" MODIFIED="1508981975777">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      nsroot (NS n _) = n
    </p>
    <p>
      nsroot n = n
    </p>
  </body>
</html></richcontent>
<node CREATED="1508981988794" ID="ID_507491082" MODIFIED="1508981997374" TEXT="Seems that children point to their parent"/>
</node>
</node>
<node CREATED="1508981080498" ID="ID_871956371" MODIFIED="1508981084051" TEXT="SpecialName">
<node CREATED="1508981089422" ID="ID_931339392" MODIFIED="1508981104513">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      data SpecialName = WhereN !Int !Name !Name
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| WithN !Int !Name
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| ImplementationN !Name [T.Text]
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| ParentN !Name !T.Text
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| MethodN !Name
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| CaseN !FC' !Name
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| ElimN !Name
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| ImplementationCtorN !Name
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| MetaN !Name !Name
    </p>
    <p>
      &#160;&#160;deriving (Eq, Ord, Data, Generic, Typeable)
    </p>
  </body>
</html></richcontent>
</node>
</node>
<node CREATED="1508981814947" ID="ID_1224920661" MODIFIED="1508981826910" TEXT="implicitable variable?">
<node CREATED="1508981827931" ID="ID_1456942781" MODIFIED="1508981831817">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      implicitable (NS n _) = False
    </p>
    <p>
      implicitable (UN xs) | T.null xs = False
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| otherwise = isLower (T.head xs) || T.head xs == '_'
    </p>
    <p>
      implicitable (MN _ x) = not (tnull x) &amp;&amp; thead x /= '_'
    </p>
    <p>
      implicitable _ = False
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1508981836722" ID="ID_184642475" MODIFIED="1508981863511" TEXT="lowercase, or starts with _, or some other check for machine names..."/>
</node>
<node CREATED="1508982099710" ID="ID_866574756" MODIFIED="1508982118894" TEXT="addDef">
<node CREATED="1508982119906" ID="ID_1672743414" MODIFIED="1508982129980">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      &#160;-- this will overwrite already existing definitions
    </p>
    <p>
      addDef :: Name -&gt; a -&gt; Ctxt a -&gt; Ctxt a
    </p>
    <p>
      addDef n v ctxt = case Map.lookup (nsroot n) ctxt of
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;Nothing -&gt; Map.insert (nsroot n)
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;(Map.insert n v Map.empty) ctxt
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;Just xs -&gt; Map.insert (nsroot n)
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;(Map.insert n v xs) ctxt
    </p>
  </body>
</html></richcontent>
</node>
</node>
<node CREATED="1509006160257" ID="ID_879118661" MODIFIED="1509006181245" TEXT="`{{name}} looks up and gets the given name based on the current context">
<node CREATED="1509006182305" ID="ID_874253889" MODIFIED="1509006189037">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      &#955;&#928;&gt; `{quicksort}
    </p>
    <p>
      NS (UN &quot;quicksort&quot;) [&quot;Elab&quot;, &quot;Learn&quot;] : TTName
    </p>
  </body>
</html></richcontent>
<node CREATED="1509006196874" ID="ID_1627515234" MODIFIED="1509006205301" TEXT="Note that the module name is &quot;Learn.Elab&quot;"/>
</node>
</node>
</node>
<node CREATED="1508829475737" ID="ID_1145454647" MODIFIED="1508829477228" TEXT="FC">
<node CREATED="1508829478001" ID="ID_409306700" MODIFIED="1508829480101" TEXT="Source location"/>
</node>
<node CREATED="1509001792881" ID="ID_1518754213" MODIFIED="1509001795501" TEXT="Universe">
<node CREATED="1509001796818" ID="ID_1916035508" MODIFIED="1509001800509" TEXT="Not sure what this is"/>
<node CREATED="1509001801033" ID="ID_554512793" MODIFIED="1509001812668">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      data Universe = NullType | UniqueType | AllTypes
    </p>
    <p>
      &#160;&#160;deriving (Eq, Ord, Data, Generic, Typeable)
    </p>
    <p>
      
    </p>
    <p>
      instance Show Universe where
    </p>
    <p>
      &#160;&#160;&#160;&#160;show UniqueType = &quot;UniqueType&quot;
    </p>
    <p>
      &#160;&#160;&#160;&#160;show NullType = &quot;NullType&quot;
    </p>
    <p>
      &#160;&#160;&#160;&#160;show AllTypes = &quot;AnyType&quot;
    </p>
  </body>
</html></richcontent>
</node>
</node>
<node CREATED="1509001821633" ID="ID_1871264912" MODIFIED="1509001822501" TEXT="Raw">
<node CREATED="1509001823799" ID="ID_1798534772" MODIFIED="1509001843756">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      data Raw = Var Name
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| RBind Name (Binder Raw) Raw
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| RApp Raw Raw
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| RType
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| RUType Universe
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| RConstant Const
    </p>
    <p>
      &#160;&#160;deriving (Show, Eq, Ord, Data, Generic, Typeable)
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1509002989958" ID="ID_1473305149" MODIFIED="1509002995739" TEXT="Raws are TT but no types"/>
</node>
<node CREATED="1509006713352" ID="ID_238375905" MODIFIED="1509948538267" TEXT="TT">
<node CREATED="1509006715800" ID="ID_79046103" MODIFIED="1509006729923" TEXT="`(x : t)">
<node CREATED="1509006730992" ID="ID_1686017758" MODIFIED="1509006737667" TEXT="Produces TT from repl">
<node CREATED="1509006738784" ID="ID_207181435" MODIFIED="1509006739348" TEXT="ex">
<node CREATED="1509006741624" ID="ID_1089831070" MODIFIED="1509006773116">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      &#955;&#928;&gt; `(S)
    </p>
    <p>
      P (DCon 1 1)
    </p>
    <p>
      &#160;&#160;(NS (UN &quot;S&quot;) [&quot;Nat&quot;, &quot;Prelude&quot;])
    </p>
    <p>
      &#160;&#160;(Bind (MN 0 &quot;_t&quot;)
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;(Pi (P (TCon 0 0) (NS (UN &quot;Nat&quot;) [&quot;Nat&quot;, &quot;Prelude&quot;]) Erased)
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;(TType (UVar &quot;./Prelude/Nat.idr&quot; 22)))
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;(P (TCon 0 0) (NS (UN &quot;Nat&quot;) [&quot;Nat&quot;, &quot;Prelude&quot;]) Erased)) : TT
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1509006775449" ID="ID_328213861" MODIFIED="1509006780780" TEXT="`(1 : Int)"/>
</node>
</node>
</node>
<node CREATED="1509948538259" ID="ID_1692667697" MODIFIED="1509948544215" TEXT="TT data constructors">
<node CREATED="1509948481091" ID="ID_1098236552" MODIFIED="1509948482134" TEXT="P">
<node CREATED="1509948483362" ID="ID_1192469100" MODIFIED="1509948528646" TEXT="P NameType n (TT n)">
<node CREATED="1509948546635" ID="ID_470572436" MODIFIED="1509948814463" TEXT="n is the type of the identifier. This, by default, is Name"/>
<node CREATED="1509948815898" ID="ID_1243080189" MODIFIED="1509948829054" TEXT="From TT.hs">
<node CREATED="1509948829962" ID="ID_1308312833" MODIFIED="1509948835159" TEXT="The type parameter is the type of -- identifiers used for bindings and explicit named references; -- usually we use @TT &apos;Name&apos;@."/>
</node>
</node>
<node CREATED="1509948556026" ID="ID_725711540" MODIFIED="1509948604438" TEXT="P seems to be a reference to something outside, along with a type, (specified by (TT n))"/>
</node>
</node>
</node>
<node CREATED="1509003063599" ID="ID_1967083793" MODIFIED="1509003135364" TEXT="Binder">
<node CREATED="1509003135312" FOLDED="true" ID="ID_495815218" MODIFIED="1509003405464" TEXT="src">
<node CREATED="1509003067382" ID="ID_133199907" MODIFIED="1509003070898">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      -- The type parameter `b` will normally be something like `TT Name` or just
    </p>
    <p>
      -- `Raw`. We do not make a type-level distinction between TT terms that happen
    </p>
    <p>
      -- to be TT types and TT terms that are not TT types.
    </p>
    <p>
      -- | All binding forms are represented in a uniform fashion. This type only represents
    </p>
    <p>
      -- the types of bindings (and their values, if any); the attached identifiers are part
    </p>
    <p>
      -- of the 'Bind' constructor for the 'TT' type.
    </p>
    <p>
      data Binder b = Lam&#160;&#160;&#160;{ binderCount :: RigCount,
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;binderTy&#160;&#160;:: !b {-^ type annotation for bound variable-}}
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- ^ A function binding
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| Pi&#160;&#160;&#160;&#160;{ binderCount :: RigCount,
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;binderImpl :: Maybe ImplicitInfo,
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;binderTy&#160;&#160;:: !b,
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;binderKind :: !b }
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- ^ A binding that occurs in a function type
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- expression, e.g. @(x:Int) -&gt; ...@ The 'binderImpl'
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- flag says whether it was a scoped implicit
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- (i.e. forall bound) in the high level Idris, but
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- otherwise has no relevance in TT.
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| Let&#160;&#160;&#160;{ binderCount :: RigCount,
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;binderTy&#160;&#160;:: !b,
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;binderVal :: b {-^ value for bound variable-}}
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- ^ A binding that occurs in a @let@ expression
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| NLet&#160;&#160;{ binderTy&#160;&#160;:: !b,
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;binderVal :: b }
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- ^ NLet is an intermediate product in the evaluator
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- that's used for temporarily naming locals during
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- reduction. It won't occur outside the evaluator.
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| Hole&#160;&#160;{ binderTy&#160;&#160;:: !b}
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- ^ A hole in a term under construction in the
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- elaborator. If this is not filled during
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- elaboration, it is an error.
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| GHole { envlen :: Int,
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;localnames :: [Name],
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;binderTy&#160;&#160;:: !b}
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- ^ A saved TT hole that will later be converted to a
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- top-level Idris metavariable applied to all
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- elements of its local environment.
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| Guess { binderTy&#160;&#160;:: !b,
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;binderVal :: b }
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- ^ A provided value for a hole. It will later be
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- substituted - the guess is to keep it
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- computationally inert while working on other things
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- if necessary.
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| PVar&#160;&#160;{ binderCount :: RigCount,
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;binderTy&#160;&#160;:: !b }
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- ^ A pattern variable (these are bound around terms
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- that make up pattern-match clauses)
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;| PVTy&#160;&#160;{ binderTy&#160;&#160;:: !b }
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- ^ The type of a pattern binding
    </p>
    <p>
      &#160;&#160;deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Data, Generic, Typeable)
    </p>
  </body>
</html></richcontent>
</node>
</node>
<node CREATED="1509003145429" ID="ID_1895911753" MODIFIED="1509003166433" TEXT="Note that App is not a Binder, but part of raw. Lam, Pi, are binders, however"/>
<node CREATED="1509003220382" ID="ID_516882451" MODIFIED="1509003225185" TEXT="Holes, and guesses, are also binders">
<node CREATED="1509003226118" ID="ID_722695639" MODIFIED="1509003242378" TEXT="A hole is &quot;?xxx&quot; in an idris source file"/>
<node CREATED="1509003242862" ID="ID_933819340" MODIFIED="1509003266441" TEXT="A guess is a hole with some sort of proof being developed within it, where the guess may or may not equal the type of the hole, I think???"/>
</node>
<node CREATED="1509003199710" ID="ID_1804291655" MODIFIED="1509003206761" TEXT="Maybe a binder is where a var gets created"/>
<node CREATED="1509002949159" ID="ID_900294028" MODIFIED="1509002954083" TEXT="ImplicitInfo">
<node CREATED="1509002954951" ID="ID_513476028" MODIFIED="1509003425810" TEXT="Use by Pi binder">
<node CREATED="1509003427206" ID="ID_1429756815" MODIFIED="1509003448751">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      -- ^ A binding that occurs in a function type
    </p>
    <p>
      -- expression, e.g. @(x:Int) -&gt; ...@ The 'binderImpl'
    </p>
    <p>
      -- flag says whether it was a scoped implicit
    </p>
    <p>
      -- (i.e. forall bound) in the high level Idris, but
    </p>
    <p>
      -- otherwise has no relevance in TT.
    </p>
  </body>
</html></richcontent>
</node>
</node>
<node CREATED="1509002956503" ID="ID_468954832" MODIFIED="1509002965817">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      data ImplicitInfo = Impl { tcimplementation :: Bool, toplevel_imp :: Bool,
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;machine_gen :: Bool }
    </p>
    <p>
      &#160;&#160;deriving (Show, Eq, Ord, Data, Generic, Typeable)
    </p>
    <p>
      
    </p>
  </body>
</html></richcontent>
</node>
</node>
<node CREATED="1508831312646" ID="ID_1285996650" MODIFIED="1509003522463" TEXT="RigCount/RigW">
<hook NAME="accessories/plugins/ClonePlugin.properties">
<Parameters CLONE_ID="CLONE_838290886" CLONE_IDS="ID_1913907204,ID_1285996650," CLONE_ITSELF="true"/>
</hook>
<node CREATED="1508831316629" ID="ID_71738024" MODIFIED="1508831320250" TEXT="from Typecheck.hs">
<node CREATED="1508831321029" ID="ID_879289206" MODIFIED="1508831326286">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      &#160;&#160;chk :: RigCount -&gt; -- 'sigma' in Bob Atkey's QTT paper, except that
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- for implementation purposes it could be 0, 1 or omega when
    </p>
    <p>
      &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;-- checking variable usage.
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1508831377981" ID="ID_535798040" MODIFIED="1508831379673" TEXT="https://bentnib.org/quantitative-type-theory.pdf"/>
<node CREATED="1509452007600" ID="ID_169231005" MODIFIED="1509452010828" TEXT="http://idris.readthedocs.io/en/latest/reference/erasure.html"/>
</node>
<node CREATED="1509003559376" ID="ID_304283691" MODIFIED="1509003570659" TEXT="Used to determine when we can erase the type from the executable">
<node CREATED="1509003572008" ID="ID_664253535" MODIFIED="1509003598454" TEXT="To prevent things like a vector containing a nat from holding (S S S S S z) for its length variable">
<node CREATED="1509003620727" ID="ID_1469346969" MODIFIED="1509003625483" TEXT="A lot of additional memory"/>
</node>
<node CREATED="1509003598696" ID="ID_1693222228" MODIFIED="1509003616785" TEXT="Instead, length can be erased"/>
</node>
</node>
</node>
</node>
<node CREATED="1509940400558" ID="ID_1772385004" MODIFIED="1510032818959" TEXT="Modules">
<node CREATED="1509940405838" ID="ID_1215207146" MODIFIED="1509940406994" TEXT="Idris">
<node CREATED="1509940407822" ID="ID_198919298" MODIFIED="1509940408450" TEXT="Core">
<node CREATED="1509940409982" ID="ID_781260799" MODIFIED="1509940411138" TEXT="Binary">
<node CREATED="1509940418878" ID="ID_627463928" MODIFIED="1509940432833" TEXT="Used for serialization"/>
</node>
<node CREATED="1509940762990" ID="ID_219266119" MODIFIED="1509940765210" TEXT="CaseTree">
<node CREATED="1509940766222" ID="ID_1077679169" MODIFIED="1509940778082" TEXT="something about elaboration of case expressions????"/>
</node>
<node CREATED="1509940838623" ID="ID_72401012" MODIFIED="1509940841282" TEXT="Constraints">
<node CREATED="1509940910317" ID="ID_1381872182" MODIFIED="1509940917746" TEXT="solves universe constraints"/>
<node CREATED="1509940919271" ID="ID_1413978272" MODIFIED="1509941230707" TEXT="There are variables and values for type universes, and constraints &lt;= and &gt;="/>
</node>
<node CREATED="1509941571830" ID="ID_1911977109" MODIFIED="1509941574129" TEXT="DeepSeq">
<node CREATED="1509941575062" ID="ID_690705212" MODIFIED="1509941592506" TEXT="deals with finding memory leaks, and evaluating fully haskell expressions"/>
</node>
<node CREATED="1509941648334" ID="ID_794758624" MODIFIED="1509941652594" TEXT="Elaborate">
<node CREATED="1509941653527" ID="ID_506876821" MODIFIED="1509941669236" TEXT="This is the core elaborator. It differs from Idris/ElabDecls.hs and Idris/Elab/..."/>
<node CREATED="1509941670759" ID="ID_1172769198" MODIFIED="1509941677362" TEXT="Not sure how it works???"/>
</node>
<node CREATED="1509941678303" ID="ID_1643455551" MODIFIED="1509942532531" TEXT="Evaluate">
<node CREATED="1509942532526" ID="ID_1085741079" MODIFIED="1509942533563" TEXT="eval">
<node CREATED="1509942524703" ID="ID_1147566876" MODIFIED="1509942531267" TEXT="Evaluates expressions"/>
</node>
</node>
<node CREATED="1509942697703" ID="ID_1632439094" MODIFIED="1509942699667" TEXT="Execute">
<node CREATED="1509942713151" ID="ID_1336937218" MODIFIED="1509942714483" TEXT="doExec">
<node CREATED="1509942715423" ID="ID_1108154711" MODIFIED="1509942721643" TEXT="executes terms, including handling FFI"/>
</node>
</node>
<node CREATED="1509942972912" ID="ID_191774571" MODIFIED="1509942974611" TEXT="ProofState">
<node CREATED="1509943000223" ID="ID_491060430" MODIFIED="1509943011555" TEXT="deals with holes, and proofs... not sure"/>
</node>
<node CREATED="1509942996503" ID="ID_1373567474" MODIFIED="1509942998651" TEXT="ProofTerm">
<node CREATED="1509943013879" ID="ID_599605882" MODIFIED="1509943018899" TEXT="Part of ProofState??"/>
</node>
<node CREATED="1509943034375" ID="ID_1839569348" MODIFIED="1509943035043" TEXT="TT">
<node CREATED="1509943035871" ID="ID_766884649" MODIFIED="1509943039403" TEXT="The core of the system"/>
<node CREATED="1509943380775" ID="ID_424281679" MODIFIED="1509943407443" TEXT="TT contains holes, so an expression in TT is not guaranteed to be executable"/>
</node>
<node CREATED="1509943082551" ID="ID_970729173" MODIFIED="1509943084563" TEXT="Typecheck">
<node CREATED="1509943085527" ID="ID_1763655364" MODIFIED="1509943088115" TEXT="Type checks TT"/>
<node CREATED="1509943096223" ID="ID_1990803640" MODIFIED="1509943097011" TEXT="check">
<node CREATED="1509943097951" ID="ID_65658945" MODIFIED="1509943108492" TEXT="Converts Raw to a term and a type in TT"/>
</node>
</node>
<node CREATED="1509943186872" ID="ID_1878163786" MODIFIED="1509943190220" TEXT="Unify">
<node CREATED="1509943318607" ID="ID_1195702828" MODIFIED="1509943379252" TEXT="Fills in holes???"/>
</node>
<node CREATED="1509943472144" ID="ID_1073720578" MODIFIED="1509943474691" TEXT="WHNF">
<node CREATED="1509943475735" ID="ID_53463067" MODIFIED="1509943484995" TEXT="Reduces TT terms to their whnf"/>
</node>
</node>
</node>
</node>
<node CREATED="1509022257017" ID="ID_1604491212" MODIFIED="1509022265453" TEXT="Compilation/Interpretation">
<node CREATED="1509022266688" ID="ID_1015746918" MODIFIED="1509022271013" TEXT="Code can be compiled">
<node CREATED="1509022279777" ID="ID_988499204" MODIFIED="1509022284429" TEXT="multiple languages are supported">
<node CREATED="1509022657736" ID="ID_1878201304" MODIFIED="1509022658716" TEXT="c"/>
<node CREATED="1509022659168" ID="ID_1393917003" MODIFIED="1509022660220" TEXT="js"/>
<node CREATED="1509022660488" ID="ID_1664859284" MODIFIED="1509022661644" TEXT="node"/>
</node>
<node CREATED="1509022297953" ID="ID_1720977883" MODIFIED="1509022303012" TEXT=":exec will compile and run"/>
<node CREATED="1509022303456" ID="ID_309739124" MODIFIED="1509022309732" TEXT=":compile will compile"/>
</node>
<node CREATED="1509022271665" ID="ID_283247398" MODIFIED="1509022278397" TEXT="There is some sort of dynamic linker"/>
<node CREATED="1509022287097" ID="ID_1403848605" MODIFIED="1509022293884" TEXT="It can also be interpreted">
<node CREATED="1509022294721" ID="ID_1498797643" MODIFIED="1509022295733" TEXT=":x"/>
</node>
</node>
</node>
</node>
</map>
