CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "abc.sml";
use "typing.sml";
use "evaluator.sml";
use "boolexp.yacc.sig";
use "boolexp.yacc.sml";
use "boolexp.lex.sml";
use "load-boolexp.sml";
Control.Print.printLength := 1000; (* set printing parameters so that *)
Control.Print.printDepth := 1000; (* we’ll see all details *)
Control.Print.stringDepth := 1000; (* and strings *)
