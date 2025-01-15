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

import scala.util.hashing.MurmurHash3
import cats.implicits.*

trait HasherFactory[Seed, I, O] { hf =>
  def create(seed: Seed): Hasher[I, O]

  def map[O2](f: O => O2): HasherFactory[Seed, I, O2] = (seed: Seed) => hf.create(seed).map(f)
}

object HasherFactory {
  val murmur3: HasherFactory[Int, String, Int] =
    (seed: Int) => MurmurHash3.stringHash(_, seed)
}
