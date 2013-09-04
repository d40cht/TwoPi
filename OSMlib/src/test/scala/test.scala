package org.seacourt.osm.test

import org.scalatest.{FunSuite}


class SRTMTest extends FunSuite
{
    import org.seacourt.osm._
    
    test("Interpolation etc")
    {
        val t = new SRTMInMemoryTile( 1.0, 2.0, 2, 2, 1.0, Array[Short]( 1, 2, 3, 4 ) )
        
        assert( t.elevation( 1.0, 2.0 ) === Some(1.0) )
        assert( t.elevation( 1.5, 2.0 ) === Some(2.0) )
        assert( t.elevation( 1.0, 2.5 ) === Some(1.5) )
        assert( t.elevation( 1.5, 2.5 ) === Some(2.5) )
    }
}


