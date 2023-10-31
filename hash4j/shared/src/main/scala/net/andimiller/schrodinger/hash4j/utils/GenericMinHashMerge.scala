/*
 * Copyright 2023 andimiller
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

package net.andimiller.schrodinger.hash4j
package utils

import com.dynatrace.hash4j.util.PackedArray

object GenericMinHashMerge {

  /** Performs a merge of two hash4j MinHash variants into a new Byte Array
    */
  def merge(
      components: Int,
      bits: Int,
      left: Array[Byte],
      right: Array[Byte]
  ): Array[Byte] = {
    val pah    = PackedArray.getHandler(bits)
    val merged = pah.create(components)

    (0 until components).foreach { idx =>
      val l = pah.get(left, idx)
      val r = pah.get(right, idx)
      pah.set(merged, idx, Math.min(l, r))
    }
    merged
  }

}
