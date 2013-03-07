package net.emaze.networks;

import net.emaze.dysfunctional.options.Maybe;
import org.junit.Assert;
import org.junit.Test;

public class IpRangeTest {
    
    private static final Ipv4 ADDRESS = Ipv4.parse("192.168.1.128");
    private static final Ipv4 BEFORE_ADDRESS = Ipv4.parse("192.168.1.127");
    private static final Ipv4 AFTER_ADDRESS = Ipv4.parse("192.168.1.129");

    @Test
    public void nextYieldsNextAddress() {
        final Maybe<Ipv4> next = IpRange.nextOf(ADDRESS);
        Assert.assertEquals(AFTER_ADDRESS, next.value());
    }

    @Test
    public void nextAfterLastYieldsNothing() {
        final Maybe<Ipv4> next = IpRange.nextOf(Ipv4.LAST_IP);
        Assert.assertEquals(Maybe.<Ipv4>nothing(), next);
    }

    @Test
    public void previousYieldsPreviousAddress() {
        final Maybe<Ipv4> previous = IpRange.previousOf(ADDRESS);
        Assert.assertEquals(BEFORE_ADDRESS, previous.value());
    }

    @Test
    public void previousBeforeFirstYieldsException() {
        final Maybe<Ipv4> previous = IpRange.previousOf(Ipv4.FIRST_IP);
        Assert.assertEquals(Maybe.<Ipv4>nothing(), previous);
    }

    @Test
    public void offsetCanYieldAGreaterIp() {
        final Maybe<Ipv4> displaced = IpRange.offset(ADDRESS, 1);
        Assert.assertEquals(AFTER_ADDRESS, displaced.value());
    }

    @Test
    public void offsettingAfterLastIpYieldsNothing() {
        final Maybe<Ipv4> displaced = IpRange.offset(Ipv4.LAST_IP, 1);
        Assert.assertEquals(Maybe.<Ipv4>nothing(), displaced);
    }

    @Test
    public void offsetCanYieldALesserIp() {
        final Maybe<Ipv4> displaced = IpRange.offset(ADDRESS, -1);
        Assert.assertEquals(BEFORE_ADDRESS, displaced.value());
    }

    @Test
    public void offsettingBeforeFirstIpYieldsNothing() {
        final Maybe<Ipv4> displaced = IpRange.offset(Ipv4.FIRST_IP, -1);
        Assert.assertEquals(Maybe.<Ipv4>nothing(), displaced);
    }

}