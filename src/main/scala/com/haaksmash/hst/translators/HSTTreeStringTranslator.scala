package com.haaksmash.hst.translators

import com.haaksmash.hst.Node

object HSTTreeStringTranslator extends BaseTranslator {

  override def translate(document: Node) = {
    document.toString
  }
}
