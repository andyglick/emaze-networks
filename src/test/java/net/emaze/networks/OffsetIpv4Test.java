package net.emaze.networks;

import net.emaze.dysfunctional.options.Maybe;
import org.junit.Assert;
import org.junit.Test;

public class OffsetIpv4Test {

    private static final Ipv4 ADDRESS = Ipv4.parse("192.168.1.128");
    private static final Ipv4 BEFORE_ADDRESS = Ipv4.parse("192.168.1.127");
    private static final Ipv4 AFTER_ADDRESS = Ipv4.parse("192.168.1.129");
    private static final Maybe<Ipv4> NO_ADDRESS = Maybe.<Ipv4>nothing();

    @Test
    public void offsetCanYieldAGreaterIp() {
        final Maybe<Ipv4> displaced = new OffsetIpv4().perform(ADDRESS, 1L);
        Assert.assertEquals(AFTER_ADDRESS, displaced.value());
    }

    @Test
    public void offsettingAfterLastIpYieldsNothing() {
        final Maybe<Ipv4> displaced = new OffsetIpv4().perform(Ipv4.LAST_IP, 1L);
        Assert.assertEquals(NO_ADDRESS, displaced);
    }

    @Test
    public void offsetCanYieldALesserIp() {
        final Maybe<Ipv4> displaced = new OffsetIpv4().perform(ADDRESS, -1L);
        Assert.assertEquals(BEFORE_ADDRESS, displaced.value());
    }

    @Test
    public void offsettingBeforeFirstIpYieldsNothing() {
        final Maybe<Ipv4> displaced = new OffsetIpv4().perform(Ipv4.FIRST_IP, -1L);
        Assert.assertEquals(NO_ADDRESS, displaced);
    }
    
    @Test
    public void zeroOffsetYieldsSame() {
        final Maybe<Ipv4> displaced = new OffsetIpv4().perform(ADDRESS, 0L);
        Assert.assertEquals(ADDRESS, displaced.value());
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullIpThrowsException() {
        new OffsetIpv4().perform(null, 0L);
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullOffsetThrowsException() {
        new OffsetIpv4().perform(ADDRESS, null);
    }
}