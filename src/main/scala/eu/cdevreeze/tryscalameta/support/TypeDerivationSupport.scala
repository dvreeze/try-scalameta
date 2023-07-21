/*
 * Copyright 2022-2022 Chris de Vreeze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package eu.cdevreeze.tryscalameta.support

import scalafix.v1._

/**
 * Support for getting the super-types of a type.
 *
 * See for example https://scalacenter.github.io/scalafix/docs/developers/symbol-information.html.
 *
 * @author
 *   Chris de Vreeze
 */
object TypeDerivationSupport {

  def getParentSymbolsOrSelf(symbol: Symbol)(implicit doc: SemanticDocument): List[Symbol] = {
    symbol.info match {
      case None => List(symbol)
      case Some(symbolInfo) =>
        symbolInfo.signature match {
          case ClassSignature(_, parents, _, _) =>
            List(symbol).appendedAll {
              parents
                .collect { case TypeRef(_, parentSymbol, _) =>
                  // Recursive call
                  getParentSymbolsOrSelf(parentSymbol)(doc)
                }
                .flatten
                .distinct
            }
          case _ => List(symbol)
        }
    }
  }

}
