package com.avsystem.commons
package rpc

abstract class RawValueCompanion[Raw] {
  type AsRaw[Real] = rpc.AsRaw[Raw, Real]
  type AsReal[Real] = rpc.AsReal[Raw, Real]
  type AsRawReal[Real] = rpc.AsRawReal[Raw, Real]
}
