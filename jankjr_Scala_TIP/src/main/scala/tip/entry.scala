package tip

import java.io.{ PrintWriter, FileFilter, File, BufferedReader, InputStreamReader }
import org.parboiled2.ParseError
import tip.analysis.{ TypeAnalysis, DeclarationAnalysis }
import tip.graph.IntraControlFlowGraph
import tip.AST._
import tip.parser.TipParser
import scala.io.Source
import scala.util.{ Failure, Success }
import tip.run._

class Option {
  var cfg = false;
  var types = false;
  var interpret = false;
  var source: File = null
  var out: File = Globals.defaultOut

  def check(): Boolean = {
    source != null
  }
}

object entry {

  def printUsage() = {
    println("""
               | Usage:
               | tip <options> <source> [out]
               |
               | <source> can be a file or a directory,
               |
               | [out] is an optional custom output directory,
               | by default ./out is used
               |
               | possible options are:
               | -cfg : to output the control flow graph
               | -types : to output the cfg with the types at the declaration nodes
               |
             """.stripMargin)

  }

  def main(args: Array[String]): Unit = {

    val options = new Option()

    var i = 0
    while (i < args.length) {
      args(i) match {
        case "-cfg" =>
          options.cfg = true
        case "-types" =>
          options.types = true
        case "-interp" =>
          options.interpret = true
        case "-inp" =>
          val s : BufferedReader = new BufferedReader( new InputStreamReader( System.in ) );
          System.out.println( "Input file:" )
          val source = s.readLine()
          options.source = new File(source)
          
          i = args.length;
    
        case s: String =>
          if (i == args.length - 1 && options.source != null)
            options.out = new File(s)
          else if (i == args.length - 1 && options.source == null)
            options.source = new File(s)
          else if (i == args.length - 2)
            options.source = new File(s)
          else
            println(s"Unrecognised option $s")
      }
      i += 1
    }

    if (!options.check())
      printUsage()
    else {

      val sources = if (options.source.isDirectory) {
        options.source.listFiles(new FileFilter {
          def accept(fl: File): Boolean = fl.getName.endsWith(".tip")
        })
      } else {
        Array(options.source)
      }

      sources.foreach { file =>
        println(s"\nProcessing ${file.getName}")

        val program = Source.fromFile(file).mkString
        val tipParser = new TipParser(program)
        val res = tipParser.InputLine.run()

        res match {
          case Failure(e: ParseError) =>
            println(s"Failure parsing the program: $file\n$program")
            println(tipParser.formatError(e, showTraces = true));
          case Failure(e: Throwable) =>
            println(s"Failure parsing the program: $file\n$program")
          case Success(programNode) =>
            try {
              DeclarationAnalysis(programNode)
              val ta = TypeAnalysis(programNode)

              if (options.cfg) {
                val cfgs = IntraControlFlowGraph.generateFromProgram(programNode, IntraControlFlowGraph.prettyLabeller)

                cfgs.foreach { cfg =>
                  Utils.output(file, cfg._1.name.toString(), OutputKind.Cfg, cfg._2.toDot(), options.out)
                }
              }
              
              if (options.interpret) {
                val env = new Interpreter();
                env.run(programNode);
              }
              
              if (options.types) {
                val ttip = programNode.toTypedString()
                //Utils.output(file, "", OutputKind.Ast, ttip, options.out)
                
                val cnst = ta.generatedConstraints()
                val cnstOut = cnst.mkString("\n")
                println(cnstOut);
                println(ttip);
                //Utils.output(file, "", OutputKind.Constraints, cnstOut, options.out)
              }
            } catch {
              case e: Exception => println(s"Error processing $file\n$e")
            }
        }
      }

      println("Success")
    }
  }
}
