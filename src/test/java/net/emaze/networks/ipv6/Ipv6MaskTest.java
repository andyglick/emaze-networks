package net.emaze.networks.ipv6;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.math.BigInteger;
import net.emaze.dysfunctional.order.Order;
import net.emaze.networks.FixedSizeNatural;
import org.junit.Assert;
import org.junit.Test;

public class Ipv6MaskTest {

    @Test
    public void zeroMaskBitsAreAllZeroes() {
        final Ipv6Mask mask = Ipv6Mask.net(0);
        Assert.assertEquals(FixedSizeNatural.zero(128), mask.bits());
    }

    @Test
    public void fullMaskBitsAreAllOnes() {
        final Ipv6Mask mask = Ipv6Mask.net(128);
        Assert.assertEquals(FixedSizeNatural.biggest(128), mask.bits());
    }

    @Test
    public void maskHasSizeAmountBitsSetToOne() {
        final int size = 87;
        final Ipv6Mask mask = Ipv6Mask.net(size);
        Assert.assertEquals(size, mask.bits().bitCount());
    }

    @Test
    public void maskEndsWithZeroes() {
        final int size = 87;
        final Ipv6Mask mask = Ipv6Mask.net(size);
        Assert.assertEquals(FixedSizeNatural.biggest(128).shiftLeft(128 - size), mask.bits());
    }

    @Test
    public void hostMaskIsComplementaryToNetMask() {
        final int size = 8;
        final Ipv6Mask mask = Ipv6Mask.host(size);
        Assert.assertEquals(FixedSizeNatural.biggest(128).shiftRight(128 - size), mask.hostBits());
    }

    @Test
    public void maskWithSameSizeAreEqual() {
        final int size = 87;
        final Ipv6Mask mask = Ipv6Mask.net(size);
        final Ipv6Mask otherMask = Ipv6Mask.net(size);
        Assert.assertEquals(Order.EQ, Order.of(mask.compareTo(otherMask)));
    }

    @Test
    public void widenHostYieldsGreaterMask() {
        final int size = 87;
        final Ipv6Mask mask = Ipv6Mask.net(size);
        final Ipv6Mask otherMask = mask.widenHosts();
        Assert.assertEquals(Order.GT, Order.of(mask.compareTo(otherMask)));
    }

    @Test
    public void narrowHostYieldsSmallerMask() {
        final int size = 87;
        final Ipv6Mask mask = Ipv6Mask.net(size);
        final Ipv6Mask otherMask = mask.narrowHosts();
        Assert.assertEquals(Order.LT, Order.of(mask.compareTo(otherMask)));
    }

    @Test
    public void narrowHostOfNarrowestHasMaxSize() {
        final Ipv6Mask mask = Ipv6Mask.getNarrowestMask().narrowHosts();
        Assert.assertEquals(Ipv6Mask.getNarrowestMask().population(), mask.population());
    }

    @Test
    public void widenHostOfWidestHasZeroSize() {
        final Ipv6Mask mask = Ipv6Mask.getWidestMask().widenHosts();
        Assert.assertEquals(Ipv6Mask.getWidestMask().population(), mask.population());
    }

    @Test
    public void netAndHostMasksAreComplementar() {
        final int size = 87;
        final Ipv6Mask mask = Ipv6Mask.net(size);
        final Ipv6Mask otherMask = Ipv6Mask.host(128 - size);
        Assert.assertEquals(mask, otherMask);
    }

    @Test(expected = IllegalArgumentException.class)
    public void throwsIfSizeExceedsNumberOfBits() {
        Ipv6Mask.net(129);
    }

    @Test(expected = IllegalArgumentException.class)
    public void throwsIfSizeAreNegative() {
        Ipv6Mask.net(-1);
    }

    @Test
    public void atHomeYouCanHaveAnInternetOfInternets() {//Now your washing machine can have an PUBLIC IP for all your pants, t-shirts, and (web)socks!
        final Ipv6Mask mask = Ipv6Mask.net(64);
        Assert.assertEquals(new BigInteger("10000000000000000", 16), mask.hosts());
    }

    @Test
    public void hostPopulationIsComplemntaryToMaskSize() {
        final Ipv6Mask mask = Ipv6Mask.net(1);
        Assert.assertEquals(127, mask.hostPopulation());
    }

    @Test
    public void maskIsWidestWhenSizeIsZero() {
        final Ipv6Mask mask = Ipv6Mask.net(0);
        Assert.assertTrue(mask.isWidest());
    }

    @Test
    public void maskIsNarrowestWhenPopulationIsZero() {
        final Ipv6Mask mask = Ipv6Mask.host(0);
        Assert.assertTrue(mask.isNarrowest());
    }
        @Test
    public void canSerializeAndDeserialize() throws IOException, ClassNotFoundException {
        final Ipv6Mask value = Ipv6Mask.net(64);
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        final ObjectOutputStream oos = new ObjectOutputStream(baos);
        oos.writeObject(value);
        final ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(baos.toByteArray()));
        final Object got = ois.readObject();
        Assert.assertEquals(value, got);
    }
}
