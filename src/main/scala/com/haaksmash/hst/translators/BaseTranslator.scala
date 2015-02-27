package com.haaksmash.hst.translators

import com.haaksmash.hst.Document

trait BaseTranslator {

  def translate(document: Document): String

}
