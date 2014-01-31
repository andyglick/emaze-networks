package net.emaze.networks.my;

import junit.framework.Assert;
import net.emaze.dysfunctional.order.Order;
import org.junit.Ignore;
import org.junit.Test;

public class IpTest {

    private static final IpPolicy V4 = new IpPolicy.V4();
    private static final int V4_LOCALHOST_AS_INT = 2130706433;
    private static final Ip V4_ADDRESS = IpParsers.parse("127.0.0.1");
    private static final Ip V4_BEFORE_ADDRESS = IpParsers.parse("127.0.0.0");
    private static final Ip V4_AFTER_ADDRESS = IpParsers.parse("127.0.0.2");

    @Test
    public void sameIpV4AreEqual() {
        final Ip anAddress = IpParsers.parseFromBitsV4(V4_LOCALHOST_AS_INT);
        final Ip sameAddress = IpParsers.parseFromBitsV4(V4_LOCALHOST_AS_INT);
        Assert.assertEquals(anAddress, sameAddress);
    }
    
    @Test
    public void sameIpRepresentedAsV4AndV6areEqual() {
        final Ip anAddress = IpParsers.parseFromStringV6("::FFFF:127.0.0.1");
        final Ip sameAddress = IpParsers.parseFromStringV4("127.0.0.1");
        Assert.assertEquals(anAddress, sameAddress);
    }

    @Test
    public void sameIpV4InDifferentFormatsAreEquals() {
        Assert.assertEquals(IpParsers.parseFromBitsV4(V4_LOCALHOST_AS_INT), IpParsers.parse("127.0.0.1"));
    }

    @Test
    public void differentIpsAreNotEqual() {
        Assert.assertEquals(false, V4_ADDRESS.equals(V4_AFTER_ADDRESS));
    }
    
    @Test
    public void ipIsDifferentFromNull() {
        Assert.assertEquals(false, V4_ADDRESS.equals(null));
    }

    @Test
    public void ipIsDifferentFromOtherObjects() {
        Assert.assertEquals(false, V4_ADDRESS.equals(new Object()));
    }

    @Test
    public void comparingWithSameYieldsEqual() {
        final Ip anAddress = IpParsers.parseFromBitsV4(1);
        final Ip sameAddress = IpParsers.parseFromBitsV4(1);
        Assert.assertEquals(Order.EQ.order(), anAddress.compareTo(sameAddress));
    }

    @Test
    public void comparingWithNextYieldsGreaterThan() {
        Assert.assertEquals(Order.GT.order(), V4_AFTER_ADDRESS.compareTo(V4_ADDRESS));
    }

    @Test
    public void comparingWithPreviousYieldsLesserThan() {
        Assert.assertEquals(Order.LT.order(), V4_BEFORE_ADDRESS.compareTo(V4_ADDRESS));
    }

    @Test
    public void firstIpIsLessThanLastIp() {
        Assert.assertEquals(Order.LT.order(), V4.getFirstIp().compareTo(V4.getLastIp()));
    }

    @Test(expected = IllegalArgumentException.class)
    public void comparingWithNullThrows() {
        V4_ADDRESS.compareTo(null);
    }

    @Test
    public void offsetCanYieldAGreaterIp() {
        final Ip displaced = V4_ADDRESS.next();
        Assert.assertEquals(V4_AFTER_ADDRESS, displaced);
    }

    @Test
    public void nextOfLastIsItself() {
        Assert.assertEquals(V4.getLastIp(), V4.getLastIp().next());
    }

    @Test
    public void offsetCanYieldALesserMyIp() {
        final Ip displaced = V4_ADDRESS.previous();
        Assert.assertEquals(V4_BEFORE_ADDRESS, displaced);
    }

    @Test
    public void previousOfFirstIsItself() {
        Assert.assertEquals(V4.getFirstIp(), V4.getFirstIp().previous());
    }

    @Test
    public void maskingAnAddressYieldsNetworkPart() {
        final Ip address = IpParsers.parse("192.168.1.123");
        final Mask netmask = new Mask(16, new IpPolicy.V4());
        final Ip expected = IpParsers.parse("192.168.0.0");
        Assert.assertEquals(expected, address.mask(netmask));
    }

    @Test(expected = IllegalArgumentException.class)
    public void maskingWithNullThrows() {
        V4_ADDRESS.mask(null);
    }

    @Test
    @Ignore
    public void toStringRendersCorrectlyWhenHighestBitOn() {
        Assert.assertEquals("255.255.255.255", V4.getLastIp().toString());
    }

    @Test
    @Ignore
    public void toStringRendersCorrectlyWhenHighestBitOff() {
        Assert.assertEquals("0.0.0.0", V4.getFirstIp().toString());
    }
}
