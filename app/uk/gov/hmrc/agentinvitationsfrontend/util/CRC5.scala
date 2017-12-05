/*
 * Copyright 2017 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.agentinvitationsfrontend.util

import java.nio.charset.Charset

class CRC5{

  /**
    * Implements a "normal" MSB-first byte-width CRC5 function using a lookup table.
    * See the <a href="http://reveng.sourceforge.net/crc-catalogue/">Catalogue of
    * parametrised CRC algorithms</a> for more information on these algorithms and
    * others.
    */
  object CRC5 {

    /* Params for CRC-5/EPC */
    val bitWidth = 5
    val poly = 0x09
    val initial = 0x09
    val xorOut = 0

    val table: Seq[Int] = {
      val widthMask = (1 << bitWidth) - 1
      val shpoly = poly << (8 - bitWidth)
      for (i <- 0 until 256) yield {
        var crc = i
        for (_ <- 0 until 8) {
          crc = if ((crc & 0x80) != 0) (crc << 1) ^ shpoly else crc << 1
        }
        (crc >> (8 - bitWidth)) & widthMask
      }
    }

    val ASCII = Charset.forName("ASCII")

    def calculate(string: String): Int = calculate(string.getBytes())

    def calculate(input: Array[Byte]): Int = {
      val start = 0
      val length = input.length
      var crc = initial ^ xorOut
      for (i <- 0 until length) {
        crc = table((crc << (8 - bitWidth)) ^ (input(start + i) & 0xff)) & 0xff
      }
      crc ^ xorOut
    }
  }
}
