package net.emaze.networks;

import junit.framework.Assert;
import net.emaze.dysfunctional.order.Order;
import org.junit.Test;

public class Ipv4Test {

    private static final long LOCALHOST_AS_LONG = 2130706433L;
    private static final Ipv4 ADDRESS = Ipv4.parse("127.0.0.1");
    private static final Ipv4 BEFORE_ADDRESS = Ipv4.parse("127.0.0.0");
    private static final Ipv4 AFTER_ADDRESS = Ipv4.parse("127.0.0.2");

    @Test
    public void sameIpv4sAreEqual() {
        final Ipv4 anAddress = Ipv4.fromLong(LOCALHOST_AS_LONG);
        final Ipv4 sameAddress = Ipv4.fromLong(LOCALHOST_AS_LONG);
        Assert.assertEquals(anAddress, sameAddress);
    }

    @Test
    public void sameIpInDifferentFormatsAreEquals() {
        Assert.assertEquals(Ipv4.fromLong(LOCALHOST_AS_LONG), Ipv4.parse("127.0.0.1"));
    }

    @Test
    public void differentIpv4sAreNotEqual() {
        Assert.assertEquals(false, ADDRESS.equals(AFTER_ADDRESS));
    }

    @Test
    public void ipv4IsDifferentFromNull() {
        Assert.assertEquals(false, ADDRESS.equals(null));
    }

    @Test
    public void ipv4IsDifferentFromOtherObjects() {
        Assert.assertEquals(false, ADDRESS.equals(new Object()));
    }

    @Test
    public void comparingWithSameYieldsEqual() {
        final Ipv4 anAddress = Ipv4.fromLong(1);
        final Ipv4 sameAddress = Ipv4.fromLong(1);
        Assert.assertEquals(Order.EQ.order(), anAddress.compareTo(sameAddress));
    }

    @Test
    public void comparingWithNextYieldsGreaterThan() {
        Assert.assertEquals(Order.GT.order(), AFTER_ADDRESS.compareTo(ADDRESS));
    }

    @Test
    public void comparingWithPreviousYieldsLesserThan() {
        Assert.assertEquals(Order.LT.order(), BEFORE_ADDRESS.compareTo(ADDRESS));
    }

    @Test(expected = IllegalArgumentException.class)
    public void comparingWithNullThrows() {
        ADDRESS.compareTo(null);
    }

    @Test
    public void offsetCanYieldAGreaterIp() {
        final Ipv4 displaced = ADDRESS.offset(1);
        Assert.assertEquals(AFTER_ADDRESS, displaced);
    }

    @Test(expected = IllegalArgumentException.class)
    public void offsettingAfterLastIpThrows() {
        Ipv4.LAST_IP.offset(1);
    }

    @Test
    public void offsetCanYieldALesserIp() {
        final Ipv4 displaced = ADDRESS.offset(-1);
        Assert.assertEquals(BEFORE_ADDRESS, displaced);
    }

    @Test(expected = IllegalArgumentException.class)
    public void offsettingBeforeFirstIpThrows() {
        Ipv4.FIRST_IP.offset(-1);
    }

    @Test
    public void zeroOffsetYieldsSame() {
        final Ipv4 displaced = ADDRESS.offset(0);
        Assert.assertEquals(ADDRESS, displaced);
    }

    @Test
    public void maskingAnAddressYieldsNetworkPart() {
        final Ipv4 address = Ipv4.parse("192.168.1.123");
        final Mask netmask = Mask.net(16);
        final Ipv4 expected = Ipv4.parse("192.168.0.0");
        Assert.assertEquals(expected, address.mask(netmask));
    }

    @Test(expected = IllegalArgumentException.class)
    public void maskingWithNullThrows() {
        ADDRESS.mask(null);
    }
}
