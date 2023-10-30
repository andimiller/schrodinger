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

package net.andimiller.schrodinger

@FunctionalInterface
trait HashShifter[Input, Width <: Int] {
  def run(i: Input): Input
}
object HashShifter {
  implicit def hashShifter64[Width <: Int: ValueOf]
      : HashShifter[Long, Width] = {
    require(
      valueOf[Width] <= 64,
      s"attempted to truncate a hash of 64 bits down to ${valueOf[Width]} bits"
    )
    l => l >> (64 - valueOf[Width])

  }
  implicit def hashShifter32[Width <: Int: ValueOf]: HashShifter[Int, Width] = {
    require(
      valueOf[Width] <= 32,
      s"attempted to truncate a hash of 32 bits down to ${valueOf[Width]} bits"
    )
    i => i >> (32 - valueOf[Width])
  }
}
