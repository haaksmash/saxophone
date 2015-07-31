/*
 * saxophone - a markup processing program
 * Copyright (C) 2015  Haak Saxberg
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.haaksmash.saxophone.translators

import com.haaksmash.saxophone.primitives._

import scala.collection.immutable.ListMap


class HTMLTranslator(
  allow_raw_strings: Boolean=true,
  footnote_as_title_text: Boolean=false
) extends NodeTranslator {

  var footnotes = Seq[String]()

  /*
   * These are block-level nodes; i.e., nodes that should have their
   * children translated recursively.
   */
  def heading(node:Heading) = s"""<h${node.level}>${translate(node)}</h${node.level}>"""
  def paragraph(node:Paragraph) = s"""<p>${translate(node)}</p>"""
  def code(node:Code) = {
    val code_contents = escapeTextForHTML(node.contents)

    s"""<figure class="code"><code${node.directives.foldLeft(""){case (s, (k, v)) => s"""$s $k="$v""""}}>$code_contents</code></figure>"""
  }

  def quote(node:Quote) = {
    if (node.source.isDefined)
      s"""<figure class="quote"><blockquote>${translate(node)}</blockquote><figcaption>${node.source.get.map(translate(_)).mkString}</figcaption></figure>"""
    else
      s"""<figure class="quote"><blockquote>${translate(node)}</blockquote></figure>"""
  }

  def orderedList(node:OrderedList) = {
    val list_items = node.items.map(li => s"<li>${li.map(translate(_)).mkString}</li>") mkString ""
    if (node.present_unordered)
      s"""<ul>$list_items</ul>"""
    else
      s"""<ol>$list_items</ol>"""
  }

  def unorderedList(node:UnorderedList) = {
    val list_items = node.items.map(li => s"""<li>${li.map(translate(_)).mkString}</li>""") mkString ""
    s"""<ul>$list_items</ul>"""
  }

  def footnote(node:Footnote) = {
    val footnote_number = footnotes.size + 1
    if (footnote_as_title_text) {
      footnotes = footnotes :+ ""
      val title_text = translate(node).replaceAll("\"", "\\\\\"")
      s"""<a title="${title_text}" rel="footnote">$footnote_number</a>"""
    } else {
      footnotes = footnotes :+ translate(node)
      s"""<a href="#note:$footnote_number" name="rn:$footnote_number" rel="footnote">$footnote_number</a>"""
    }
  }

  /*
   * Inline nodes; i.e., nodes that don't have children, but only capture
   * meta about their contents.
   */
  def link(node:Link) = s"""<a href="${node.to}">${translate(node)}</a>"""
  def emphasizedText(node:EmphasizedText) = s"<em${convertMetaToHTMLAttrs(node.meta)}>${escapeTextForHTML(node.text)}</em>"
  def forcedNewLine(node:ForcedNewline) = "<br/>"
  def standardText(node:StandardText) = escapeTextForHTML(node.text)
  def struckthroughText(node:StruckthroughText) = s"<s${convertMetaToHTMLAttrs(node.meta)}>${escapeTextForHTML(node.text)}</s>"
  def markedText(node:MarkedText) = s"""<mark${convertMetaToHTMLAttrs(node.meta)}>${escapeTextForHTML(node.text)}</mark>"""
  def weightedText(node:WeightedText) = s"<strong${convertMetaToHTMLAttrs(node.meta)}>${escapeTextForHTML(node.text)}</strong>"
  def monospacedText(node:MonospaceText) = {
    s"""<code${convertMetaToHTMLAttrs(node.meta)}>${escapeTextForHTML(node.text)}</code>"""
  }
  def rawText(node: RawText) = if (allow_raw_strings) node.text else escapeTextForHTML(node.text)

  def embed(node:EmbedNode) = node match {
    case ImageEmbedNode(arguments, meta) => s"""<img src="${arguments.head}" alt="${meta.getOrElse("alt", "")}"/>"""
    case VideoEmbedNode(arguments, meta) => arguments.head match {
      case "youtube" =>
        s"""<iframe id="ytplayer" class="ytplayer" type="text/html" src="http://www.youtube.com/embed/${arguments(1)}?autoplay=0" frameborder="0"/>"""
      case "vimeo" => s"""<iframe class="vimeoplayer" src="https://player.vimeo.com/video/${arguments(1)}" frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>"""
      case src => s"""<video>${arguments.map(src => "<source src=\""+src+"\" type=\"video/"+src.split('.').last+"\"")}Whoops, your browser doesn't support the video tag!</video>"""
    }
    case TweetEmbedNode(arguments, meta) => s"""<blockquote class="twitter-tweet"><a href="https://twitter.com/${arguments(0)}/status/${arguments(1)}">tweet by @${arguments(0)}</a></blockquote>"""
    case _ => ""
  }


  override def translate(node: Node): String = {
    val s = super.translate(node)

    val footer_string = node match {
      case n:Document if !footnotes.isEmpty && !(footnote_as_title_text) =>
        "<footer>" + footnotes.zipWithIndex.map { case (note, num) => s"""<p><a class="fnote" href="#rn:${num + 1}" name="note:${num + 1}">${num + 1}</a> $note</p>"""}.mkString + "</footer>"
      case _ => ""
    }

    s + footer_string
  }

  /**
   * Escapes a string so that it's safe for HTML; e.g., replacing {@code <} with {@code &amp;lt;}.
   * @param text the string that needs escaping
   * @return escaped version of the input
   */
  private def escapeTextForHTML(text:String):String = {
    // Order is important here, so we use a ListMap to preserve it.
    val chars_to_escape_sequence = ListMap(
      "&" -> "&amp;", // must be first so we don't accidentally kill anything in the below
      "<" -> "&lt;",
      ">" -> "&gt;",
      "---" -> "&mdash;", // but really you should just use the correct character, —.
      "--" -> "&ndash;" // same here; just use the en-dash character directly.
    )

    var new_text = text
    for (char <- chars_to_escape_sequence.keys){
      new_text = new_text.replaceAll(char, chars_to_escape_sequence(char))
    }
    new_text
  }

  private def convertMetaToHTMLAttrs(meta:Map[String, String]): String = {
    val meta_string = meta.map{case (k,v) => s"""$k="$v""""}.mkString(" ")
    if (!meta_string.isEmpty)
      " " + meta_string
    else
      ""
  }

}

object HTMLTranslator {
  def translate(node:Node): String = {
    new HTMLTranslator().translate(node)
  }
}
