package net.emaze.networks;

import java.math.BigInteger;
import net.emaze.dysfunctional.order.Order;
import org.junit.Assert;
import org.junit.Test;

public class Ipv6MaskTest {

    @Test
    public void zeroMaskBitsAreAllZeroes() {
        final Ipv6Mask mask = Ipv6Mask.net(0);
        Assert.assertEquals(BigInteger.ZERO, mask.bits());
    }

    @Test
    public void fullMaskBitsAreAllOnes() {
        final Ipv6Mask mask = Ipv6Mask.net(Ipv6Mask.BITS);
        Assert.assertEquals(Ipv6Mask.BITS, mask.bits().bitCount());
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
        Assert.assertEquals(Ipv6Mask.BITS - size, mask.bits().getLowestSetBit());
    }

    @Test
    public void hostMaskIsComplementaryToNetMask() {
        final int size = 8;
        final Ipv6Mask mask = Ipv6Mask.host(size);
        Assert.assertEquals(new BigInteger("FF", 16), mask.hostMaskBits());
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
        final Ipv6Mask mask = Ipv6Mask.NARROWEST.narrowHosts();
        Assert.assertEquals(Ipv6Mask.NARROWEST.size(), mask.size());
    }

    @Test
    public void widenHostOfWidestHasZeroSize() {
        final Ipv6Mask mask = Ipv6Mask.WIDEST.widenHosts();
        Assert.assertEquals(Ipv6Mask.WIDEST.size(), mask.size());
    }

    @Test
    public void netAndHostMasksAreComplementar() {
        final int size = 87;
        final Ipv6Mask mask = Ipv6Mask.net(size);
        final Ipv6Mask otherMask = Ipv6Mask.host(Ipv6Mask.BITS - size);
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
}
