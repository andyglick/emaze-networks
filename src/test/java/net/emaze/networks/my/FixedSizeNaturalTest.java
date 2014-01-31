package net.emaze.networks.my;

import java.math.BigInteger;
import java.util.Random;
import org.junit.Test;
import org.junit.Assert;
import org.junit.Ignore;

public class FixedSizeNaturalTest {

    @Test
    public void canConstructANaturalWithLengthAllignedToRappresentation() {
        final int[] intArray = new int[1];
        final int length = 32;
        final FixedSizeNatural got = new FixedSizeNatural(intArray, length);
        Assert.assertEquals(length, got.length());
    }

    @Test(expected = IllegalArgumentException.class)
    public void constructANaturalWithLengthNotAllignedToRappresentationThrows() {
        final int[] intArray = new int[1];
        new FixedSizeNatural(intArray, 33);
    }

    @Test(expected = IllegalArgumentException.class)
    public void canNotConstructFromEmptyByteArray() {
        final byte[] bytes = new byte[0];
        FixedSizeNatural.fromByteArray(bytes);
    }

    @Test
    public void canConstructFromByteArray() {
        final byte[] bytes = {(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF};
        final FixedSizeNatural got = FixedSizeNatural.fromByteArray(bytes);
        Assert.assertEquals(got.last(), got);
    }

    @Test
    public void toByteArrayYieldsInternalRapresentationInBytes() {
        final int[] ints = {1, 1};
        final byte[] got = new FixedSizeNatural(ints, 33).toByteArray();
        final byte[] expected = new byte[]{1, 0, 0, 0, 1};
        Assert.assertArrayEquals(expected, got);
    }

    @Test
    public void shiftLeft() {
        final int[] ints = {1};
        final FixedSizeNatural got = new FixedSizeNatural(ints, 32).shiftLeft(1);
        final int[] expectedInts = {2};
        final FixedSizeNatural expected = new FixedSizeNatural(expectedInts, 32);
        Assert.assertEquals(expected, got);
    }

    @Test
    public void shiftLeftDiscardsBitsGoneOverBitsSize() {
        final int[] ints = {1};
        final FixedSizeNatural got = new FixedSizeNatural(ints, 1).shiftLeft(1);
        final int[] expectedInts = {0};
        final FixedSizeNatural expected = new FixedSizeNatural(expectedInts, 1);
        Assert.assertEquals(expected, got);
    }

    @Test
    public void shiftLeftMovesCarryBitToMoreSignificativeInteger() {
        final int[] ints = {0, 0x80000000};
        final FixedSizeNatural got = new FixedSizeNatural(ints, 33).shiftLeft(1);
        final int[] expectedInts = {1, 0};
        final FixedSizeNatural expected = new FixedSizeNatural(expectedInts, 33);
        Assert.assertEquals(expected, got);
    }

    @Test
    public void shiftRight() {
        final int[] ints = {2};
        final FixedSizeNatural got = new FixedSizeNatural(ints, 2).shiftRight(1);
        final int[] expectedInts = {1};
        final FixedSizeNatural expected = new FixedSizeNatural(expectedInts, 2);
        Assert.assertEquals(expected, got);
    }

    @Test
    public void shiftRightMovesCarryBitToLessSignificantInteger() {
        final int[] ints = {1, 0};
        final FixedSizeNatural got = new FixedSizeNatural(ints, 33).shiftRight(1);
        final int[] expectedInts = {0, 0x80000000};
        final FixedSizeNatural expected = new FixedSizeNatural(expectedInts, 33);
        Assert.assertEquals(expected, got);
    }

    @Ignore
    @Test
    public void shiftLeftPerformanceTest() {
        final BigInteger t1 = BigInteger.probablePrime(64, new Random(System.currentTimeMillis()));
        final int shift = 19;
        final int cycles = 10000000;
        final long startBig = System.currentTimeMillis();
        for (int i = 0; i != cycles; ++i) {
            t1.shiftLeft(shift);
        }
        final long endBig = System.currentTimeMillis();

        final FixedSizeNatural t2 = FixedSizeNatural.fromByteArray(t1.toByteArray());
        final long startSeq = System.currentTimeMillis();
        for (int i = 0; i != cycles; ++i) {
            t2.shiftLeft(shift);
        }
        final long endSeq = System.currentTimeMillis();
        System.out.format("shift left -> big: %s, seq: %s%n", endBig - startBig, endSeq - startSeq);
    }

    @Ignore
    @Test
    public void shiftRightPerformanceTest() {
        final BigInteger t1 = BigInteger.probablePrime(64, new Random(System.currentTimeMillis()));
        final int shift = 19;
        final int cycles = 10000000;
        final long startBig = System.currentTimeMillis();
        for (int i = 0; i != cycles; ++i) {
            t1.shiftRight(shift);
        }
        final long endBig = System.currentTimeMillis();

        final FixedSizeNatural t2 = FixedSizeNatural.fromByteArray(t1.toByteArray());
        final long startSeq = System.currentTimeMillis();
        for (int i = 0; i != cycles; ++i) {
            t2.shiftRight(shift);
        }
        final long endSeq = System.currentTimeMillis();
        System.out.format("shift right -> big: %s, seq: %s%n", endBig - startBig, endSeq - startSeq);
    }
}
