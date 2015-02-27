package com.haaksmash.hst.translators

import com.haaksmash.hst.Document

object HSTTreeStringTranslator extends BaseTranslator {

  override def translate(document: Document) = {
    document.toString
  }
}
