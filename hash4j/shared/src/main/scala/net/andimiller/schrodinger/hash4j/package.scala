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

import com.dynatrace.hash4j.hashing.Hashing

package object hash4j extends UltraLogLogInstances {

  val wyhashFinal4Factory: HasherFactory[Long, String, Long] = seed => (input => Hashing.wyhashFinal4(seed).hashCharsToLong(input))

  val wyhashFinal4: Hasher[String, Long] =
    (input => Hashing.wyhashFinal4().hashCharsToLong(input))

}
