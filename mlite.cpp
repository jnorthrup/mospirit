/*=============================================================================
    Copyright (c) 2001-2011 Hartmut Kaiser
    Copyright (c) 2001-2011 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
///////////////////////////////////////////////////////////////////////////////
//
//  Plain mlite example demonstrating the grammar. The parser is a
//  syntax checker only and does not do any semantic evaluation.
//
//  [ JDG May 10, 2002 ]        spirit1
//  [ JDG March 4, 2007 ]       spirit2
//  [ HK November 30, 2010 ]    spirit2/utree
//
///////////////////////////////////////////////////////////////////////////////

// This rather naive example demonstrates that you can pass an instance of a
// utree as the attribute for almost any grammar. As the result the utree will
// be filled with the parse tree as generated during the parsing. This is most
// of the time not what's desired, but is usually a good first step in order to
// prepare your grammar to generate a customized AST. See the calc_utree_ast
// example for a modified version of this grammar filling the attribute with a
// AST (abstract syntax tree) representing the math expression as matched from
// the input.

// #define BOOST_SPIRIT_DEBUG
#include "pch.hxx"

 namespace cset = boost::spirit::ascii;
namespace client {
    namespace qi = boost::spirit::qi;
    namespace spirit = boost::spirit;

    namespace phoenix = boost::phoenix;
    using boost::spirit::omit;
    using boost::spirit::no_skip;
    using boost::spirit::raw;
    using boost::spirit::double_;
    using boost::spirit::utree;
    using qi::lit;
    using qi::lexeme;
    using qi::on_error;
    using qi::fail;
    using cset::char_;
    using cset::string;
    using cset::digit;
    using cset::lower;
    using cset::digit;
    using cset::alpha;
    using cset::alnum;
    using namespace qi::labels;
    using phoenix::construct;
    using phoenix::val;
    using qi::symbols;

    ///////////////////////////////////////////////////////////////////////////////
    //  Our mlite grammar
    ///////////////////////////////////////////////////////////////////////////////

    template <typename Iterator>
    struct mlite : qi::grammar<Iterator, cset::space_type, spirit::utree()> {

        mlite() : mlite::base_type(package_decl) {
            using qi::uint_;
            using qi::char_;

            KEYWORDS_TABLE = "algorithm", "and", "annotation", "assert", "block", "break", "class", "connect", "connector", "constant", "constrainedby", "der", "discrete", "each", "else", "elseif",
                    "elsewhen", "encapsulated", "end", "enumeration", "equation", "expandable", "extends", "external", "false", "final", "flow", "for", "function", "import", "if", "in", "initial", "inner",
                    "input", "loop", "model", "not", "or", "outer", "output", "package", "parameter", "partial", "protected", "public", "record", "redeclare", "replaceable", "return", "then", "true",
                    "type", "when", "while", "within"; 
            package_decl = -(lit("within") >> *alnum >> SEMICOLON) >> class_decl;
            class_decl = *(-lit("final") >> *alnum >> SEMICOLON);
            IDENT=lexeme[(char_("_")|alpha)>>*(char_("_")|alnum)];
            name=IDENT%DOT;
            STRING=lexeme['"'>>(*char_-'"')>>'"']; 
            UNSIGNED_NUMBER= (!lit('-'))>>double_; 
        }

        qi::symbols< > KEYWORDS_TABLE;

        qi::rule<Iterator, cset::space_type, spirit::utree() > package_decl , class_decl,STRING,IDENT,name,UNSIGNED_NUMBER,
        DOT = lit('.'),
        COLON = lit(':'),
        DER = lit("der"),
        ELSE = lit("else"),
        IF = lit("if"),
        WHEN = lit("when"),
        WHILE = lit("while"),
        THEN = lit("then"),
        FOR = lit("for"),
        END = lit("end"),
        OR = lit("or"),
        AND = lit("and"),
        FINAL = lit("final"),
        COMMA = lit(','),
        EQ = lit('='),
        SEMICOLON = lit(';'),
        LOOP = lit("loop"),
        ELSEWHEN = lit("elsewhen"),
        ELSEIF = lit("elseif"),
        FUNCTION = lit("function"),
        CLASS = lit("class"),
        INITIAL = lit("initial")
                ;


    };
}

///////////////////////////////////////////////////////////////////////////////
//  Main program
///////////////////////////////////////////////////////////////////////////////

int main() {
    std::cout << "/////////////////////////////////////////////////////////\n\n";
    std::cout << "Expression parser...\n\n";
    std::cout << "/////////////////////////////////////////////////////////\n\n";
    std::cout << "Type an expression...or [q or Q] to quit\n\n";

    using cset::space;
    using boost::spirit::utree;
    typedef std::string::const_iterator iterator_type;
    typedef client::mlite<iterator_type> mlite;

    mlite calc; // Our grammar

    std::string str;
    while (std::getline(std::cin, str)) {
        if (str.empty() || str[0] == 'q' || str[0] == 'Q')
            break;

        std::string::const_iterator iter = str.begin();
        std::string::const_iterator end = str.end();
        utree ut;
        bool r = phrase_parse(iter, end, calc, space, ut);

        if (r && iter == end) {
            std::cout << "-------------------------\n";
            std::cout << "Parsing succeeded: " << ut << "\n";
            std::cout << "-------------------------\n";
        } else {
            std::string rest(iter, end);
            std::cout << "-------------------------\n";
            std::cout << "Parsing failed\n";
            std::cout << "stopped at: \": " << rest << "\"\n";
            std::cout << "-------------------------\n";
        }
    }

    std::cout << "Bye... :-) \n\n";
    return 0;
}

