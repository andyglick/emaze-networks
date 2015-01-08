package net.emaze.networks;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.math.BigInteger;
import java.util.Random;
import net.emaze.dysfunctional.order.Order;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

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

    @Test(expected = IllegalArgumentException.class)
    public void constructWithUnusedBitSetThrows() {
        final int[] bytes = new int[]{0xFF};
        new FixedSizeNatural(bytes, 1);
    }

    @Test
    public void canConstructFromByteArray() {
        final byte[] bytes = {(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF};
        final FixedSizeNatural got = FixedSizeNatural.fromByteArray(bytes);
        Assert.assertEquals(new FixedSizeNatural(new int[]{0xFFFFFFFF}, 32), got);
    }

    @Test
    public void canConstructFromLong() {
        final FixedSizeNatural got = FixedSizeNatural.of(Long.MAX_VALUE);
        Assert.assertEquals(FixedSizeNatural.biggest(Long.SIZE - 1), got);
    }

    @Test
    public void canConstructZero() {
        final int length = 1;
        final FixedSizeNatural zero = FixedSizeNatural.zero(length);
        final FixedSizeNatural expected = new FixedSizeNatural(new int[]{0}, length);
        Assert.assertEquals(expected, zero);
    }

    @Test
    public void canConstructOne() {
        final int length = 1;
        final FixedSizeNatural one = FixedSizeNatural.one(length);
        final FixedSizeNatural expected = new FixedSizeNatural(new int[]{1}, length);
        Assert.assertEquals(expected, one);
    }

    @Test
    public void canConstructBiggestNatural() {
        final int length = 4;
        final FixedSizeNatural biggest = FixedSizeNatural.biggest(length);
        final FixedSizeNatural expected = new FixedSizeNatural(new int[]{0xF}, length);
        Assert.assertEquals(expected, biggest);
    }

    @Test
    public void toByteArrayYieldsInternalRapresentationInBytes() {
        final int[] ints = {1, 1};
        final byte[] got = new FixedSizeNatural(ints, 33).toByteArray();
        final byte[] expected = new byte[]{1, 0, 0, 0, 1};
        Assert.assertArrayEquals(expected, got);
    }

    @Test(expected = IllegalArgumentException.class)
    public void shiftNegativeLeftThrows() {
        FixedSizeNatural.one(32).shiftLeft(-1);
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
    public void shiftLeftOfZeroDoesntShift() {
        final FixedSizeNatural got = FixedSizeNatural.one(32).shiftLeft(0);
        Assert.assertEquals(FixedSizeNatural.one(32), got);
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

    @Test(expected = IllegalArgumentException.class)
    public void shiftNegativeRightThrows() {
        FixedSizeNatural.one(32).shiftRight(-1);
    }

    @Test
    public void shiftLeftOfMoreThanSizeReturnZero() {
        final FixedSizeNatural got = FixedSizeNatural.one(32).shiftLeft(33);
        Assert.assertEquals(FixedSizeNatural.zero(32), got);
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
    public void shiftRightOfZeroDoesntShift() {
        final FixedSizeNatural got = FixedSizeNatural.one(32).shiftRight(0);
        Assert.assertEquals(FixedSizeNatural.one(32), got);
    }

    @Test
    public void shiftRightOfMoreThanSizeReturnZero() {
        final FixedSizeNatural got = FixedSizeNatural.one(32).shiftRight(33);
        Assert.assertEquals(FixedSizeNatural.zero(32), got);
    }

    @Test
    public void shiftRightMovesCarryBitToLessSignificantInteger() {
        final int[] ints = {1, 0};
        final FixedSizeNatural got = new FixedSizeNatural(ints, 33).shiftRight(1);
        final int[] expectedInts = {0, 0x80000000};
        final FixedSizeNatural expected = new FixedSizeNatural(expectedInts, 33);
        Assert.assertEquals(expected, got);
    }

    @Test
    public void shiftRightWithAMultipleOf32ShiftsIntegers() {
        final int[] ints = {1, 0};
        final FixedSizeNatural got = new FixedSizeNatural(ints, 64).shiftRight(32);
        final int[] expectedInts = {0, 1};
        final FixedSizeNatural expected = new FixedSizeNatural(expectedInts, 64);
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canNegateNaturalBits() {
        final int cases = 2;
        final int[] ints = {0b01};
        final FixedSizeNatural expected = new FixedSizeNatural(ints, cases);
        final int[] expectedInts = {0b10};
        final FixedSizeNatural got = new FixedSizeNatural(expectedInts, cases).not();
        Assert.assertEquals(expected, got);
    }

    @Test
    public void andOfTwoNaturals() {
        final int cases = 4;
        final int[] formerInts = {0b0011};
        final int[] latterInts = {0b0101};
        final int[] expectedInts = {0b0001};
        final FixedSizeNatural expected = new FixedSizeNatural(expectedInts, cases);
        final FixedSizeNatural got = new FixedSizeNatural(formerInts, cases).and(new FixedSizeNatural(latterInts, cases));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void orOfTwoNaturals() {
        final int cases = 4;
        final int[] formerInts = {0b0011};
        final int[] latterInts = {0b0101};
        final int[] expectedInts = {0b0111};
        final FixedSizeNatural expected = new FixedSizeNatural(expectedInts, cases);
        final FixedSizeNatural got = new FixedSizeNatural(formerInts, cases).or(new FixedSizeNatural(latterInts, cases));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void xorOfTwoNaturals() {
        final int cases = 4;
        final int[] formerInts = {0b0011};
        final int[] latterInts = {0b0101};
        final int[] expectedInts = {0b0110};
        final FixedSizeNatural expected = new FixedSizeNatural(expectedInts, cases);
        final FixedSizeNatural got = new FixedSizeNatural(formerInts, cases).xor(new FixedSizeNatural(latterInts, cases));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canIncrementANumber() {
        final int[] ints = {1};
        final int size = 2;
        final FixedSizeNatural got = new FixedSizeNatural(ints, size).increment();
        final int[] expectedInts = {2};
        final FixedSizeNatural expected = new FixedSizeNatural(expectedInts, size);
        Assert.assertEquals(expected, got);
    }

    @Test
    public void incrementCarriesToMoreSignificativeChunk() {
        final int[] ints = {0, 0xFFFFFFFF};
        final int size = 33;
        final FixedSizeNatural got = new FixedSizeNatural(ints, size).increment();
        final int[] expectedInts = {1, 0};
        final FixedSizeNatural expected = new FixedSizeNatural(expectedInts, size);
        Assert.assertEquals(expected, got);
    }

    @Test
    public void incrementYieldsLastInsteadOfOverflowing() {
        final int[] ints = {0xF};
        final int size = 4;
        final FixedSizeNatural got = new FixedSizeNatural(ints, size).increment();
        final FixedSizeNatural expected = got.last();
        Assert.assertEquals(expected, got);
    }

    @Test
    public void incrementCopiesUntouchedBits() {
        final FixedSizeNatural got = FixedSizeNatural.of(1, 2, 3, 4).increment();
        final FixedSizeNatural expected = FixedSizeNatural.of(1, 2, 3, 5);
        Assert.assertEquals(expected, got);
    }

    @Test
    public void incrementDoesNotGenerateACarry() {
        final FixedSizeNatural got = FixedSizeNatural.of(1, 2, 0xffffffff, 4).increment();
        final FixedSizeNatural expected = FixedSizeNatural.of(1, 2, 0xffffffff, 5);
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canDecrementANumber() {
        final int[] ints = {2};
        final int[] expectedInts = {1};
        final int size = 2;
        final FixedSizeNatural got = new FixedSizeNatural(ints, size).decrement();
        final FixedSizeNatural expected = new FixedSizeNatural(expectedInts, size);
        Assert.assertEquals(expected, got);
    }

    @Test
    public void decrementBorrowsToLessSignificativeChunk() {
        final int[] ints = {1, 0};
        final int[] expectedInts = {0, 0xFFFFFFFF};
        final int size = 33;
        final FixedSizeNatural got = new FixedSizeNatural(ints, size).decrement();
        final FixedSizeNatural expected = new FixedSizeNatural(expectedInts, size);
        Assert.assertEquals(expected, got);
    }

    @Test
    public void decrementYieldsFirstInsteadOfUnderflowing() {
        final int[] ints = {0};
        final int size = 4;
        final FixedSizeNatural got = new FixedSizeNatural(ints, size).decrement();
        final FixedSizeNatural expected = FixedSizeNatural.zero(size);
        Assert.assertEquals(expected, got);
    }

    @Test
    public void decrementCopiesUntouchedBits() {
        final FixedSizeNatural got = FixedSizeNatural.of(1, 2, 3, 4).decrement();
        final FixedSizeNatural expected = FixedSizeNatural.of(1, 2, 3, 3);
        Assert.assertEquals(expected, got);
    }

    @Test
    public void decrementDoesNotGenerateABorrow() {
        final FixedSizeNatural got = FixedSizeNatural.of(1, 2, 0, 4).decrement();
        final FixedSizeNatural expected = FixedSizeNatural.of(1, 2, 0, 3);
        Assert.assertEquals(expected, got);
    }

    @Test
    public void firstIsZero() {
        final int[] ints = {0};
        final FixedSizeNatural expected = new FixedSizeNatural(ints, 32);
        final FixedSizeNatural got = expected.first();
        Assert.assertEquals(expected, got);
    }

    @Test
    public void lastIsTheNegationOfFirst() {
        final FixedSizeNatural expected = FixedSizeNatural.zero(32);
        final FixedSizeNatural got = FixedSizeNatural.biggest(32).not();
        Assert.assertEquals(expected, got);
    }

    @Test
    public void extendToAddsBitsToInternalRepresentation() {
        final int[] ints = {1};
        final FixedSizeNatural expected = new FixedSizeNatural(ints, 2);
        final FixedSizeNatural got = new FixedSizeNatural(ints, 1).extendTo(2);
        Assert.assertEquals(expected, got);
    }

    @Test
    public void truncateToRemovesBitsToInternalRepresentation() {
        final int[] ints = {2};
        final int[] expectedInts = {0};
        final FixedSizeNatural expected = new FixedSizeNatural(expectedInts, 1);
        final FixedSizeNatural got = new FixedSizeNatural(ints, 2).truncateTo(1);
        Assert.assertEquals(expected, got);
    }

    @Test(expected = IllegalArgumentException.class)
    public void bitWithNegativeIndexThrows() {
        final int[] ints = {0b00100000};
        final FixedSizeNatural number = new FixedSizeNatural(ints, 8);
        number.bit(-1);
    }

    @Test(expected = IllegalArgumentException.class)
    public void bitWithIndexNotLessThanLengthThrows() {
        final int[] ints = {0b00100000};
        final FixedSizeNatural number = new FixedSizeNatural(ints, 8);
        number.bit(8);
    }

    @Test
    public void bitYieldsTheBitAtTheGivenIndex() {
        final int[] ints = {0b00100000};
        final FixedSizeNatural number = new FixedSizeNatural(ints, 8);
        final boolean bit = number.bit(5);
        Assert.assertEquals(true, bit);
    }

    @Test
    public void naturalsAreEqualWhenHaveTheSameRepresentation() {
        final int[] ints = {1, 2, 3, 4};
        final FixedSizeNatural first = new FixedSizeNatural(ints, 128);
        final FixedSizeNatural second = new FixedSizeNatural(ints, 128);
        Assert.assertEquals(Order.EQ, Order.of(first.compareTo(second)));
    }

    @Test
    public void naturalsAreEqualWithSameInternalValuesEvenWhenHaveDifferentLength() {
        final int[] ints = {1, 2, 3, 4};
        final FixedSizeNatural first = new FixedSizeNatural(ints, 97);
        final FixedSizeNatural second = new FixedSizeNatural(ints, 128);
        Assert.assertEquals(Order.EQ, Order.of(first.compareTo(second)));
    }

    @Test
    public void naturalsAreEqualWithSameInternalValuesEvenWhenHaveDifferentArrayLengths() {
        final int[] firstInts = {1, 2, 3, 4};
        final int[] secondInts = {0, 1, 2, 3, 4};
        final FixedSizeNatural first = new FixedSizeNatural(firstInts, 97);
        final FixedSizeNatural second = new FixedSizeNatural(secondInts, 129);
        Assert.assertEquals(Order.EQ, Order.of(first.compareTo(second)));
    }

    @Test
    public void differentNaturalsAreComparable() {
        final int[] firstInts = {1, 2, 3, 4};
        final int[] secondInts = {1, 2, 3, 5};
        final FixedSizeNatural first = new FixedSizeNatural(firstInts, 128);
        final FixedSizeNatural second = new FixedSizeNatural(secondInts, 128);
        Assert.assertEquals(Order.LT, Order.of(first.compareTo(second)));
    }

    @Test
    public void differentNaturalsAreComparableEvenWhenHaveDifferentLength() {
        final int[] firstInts = {1, 2, 3, 4};
        final int[] secondInts = {1, 2, 3, 5};
        final FixedSizeNatural first = new FixedSizeNatural(firstInts, 97);
        final FixedSizeNatural second = new FixedSizeNatural(secondInts, 128);
        Assert.assertEquals(Order.LT, Order.of(first.compareTo(second)));
    }

    @Test
    public void differentNaturalsAreComparableEvenWhenHaveDifferentArrayLengths() {
        final int[] firstInts = {1, 2, 3, 4};
        final int[] secondInts = {0, 1, 2, 3, 5};
        final FixedSizeNatural first = new FixedSizeNatural(firstInts, 97);
        final FixedSizeNatural second = new FixedSizeNatural(secondInts, 129);
        Assert.assertEquals(Order.LT, Order.of(first.compareTo(second)));
    }

    @Test
    public void minimumComparedToMaximumIsLessThanEqual() {
        final FixedSizeNatural first = new FixedSizeNatural(new int[]{0}, 32);
        final FixedSizeNatural second = new FixedSizeNatural(new int[]{0xFFFFFFFF}, 32);
        final int order = first.compareTo(second);
        Assert.assertEquals(true, Order.of(order).isLte());
    }

    @Test
    public void canCountBits() {
        final int[] ints = {0b01010110};
        final FixedSizeNatural instance = new FixedSizeNatural(ints, 16);
        Assert.assertEquals(4, instance.bitCount());
    }

    @Test
    public void getIntValueYieldsExpected() {
        final int value = Integer.MAX_VALUE;
        Assert.assertEquals(value, FixedSizeNatural.of(value).intValue());
    }

    @Test
    public void getLongValueYieldsExpected() {
        final long value = Long.MAX_VALUE;
        Assert.assertEquals(value, FixedSizeNatural.of(value).longValue());
    }

    @Test
    public void getLongValueFromIntYieldsExpected() {
        final int value = Integer.MAX_VALUE;
        Assert.assertEquals(value, FixedSizeNatural.of(value).longValue());
    }

    @Test
    public void toStringReturnsBitsValues() {
        final FixedSizeNatural value = FixedSizeNatural.of(0xAAAAAAAA);
        Assert.assertEquals("10101010101010101010101010101010", value.toString());
    }

    @Test
    public void toStringPadsWithZeroes() {
        final FixedSizeNatural value = new FixedSizeNatural(new int[]{1, 0xAAAAAAAA}, 35);
        Assert.assertEquals("00110101010101010101010101010101010", value.toString());
    }

    @Test
    public void toStringReturnsBitsValuesSeparatedInChunksStartingFromTheRight() {
        final FixedSizeNatural value = FixedSizeNatural.of(0xAAAAAAAA);
        Assert.assertEquals("10 10101 01010 10101 01010 10101 01010", value.toString(5));
    }

    @Test
    public void canSerializeAndDeserialize() throws IOException, ClassNotFoundException {
        final FixedSizeNatural value = new FixedSizeNatural(new int[]{1, 0xAAAAAAAA}, 35);
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        final ObjectOutputStream oos = new ObjectOutputStream(baos);
        oos.writeObject(value);
        final ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(baos.toByteArray()));
        final Object got = ois.readObject();
        Assert.assertEquals(value, got);
    }

    @Ignore
    @Test
    public void shiftLeftPerformanceTest() {
        final BigInteger t1 = BigInteger.probablePrime(64, new Random(System.currentTimeMillis()));
        final int shift = 19;
        final int cycles = 10_000_000;
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
        final int cycles = 10_000_000;
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
