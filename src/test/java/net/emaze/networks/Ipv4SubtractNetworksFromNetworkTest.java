package net.emaze.networks;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import org.junit.Assert;
import org.junit.Test;

public class Ipv4SubtractNetworksFromNetworkTest {

    @Test
    public void canSubtractContainedFromContainer() {
        final Ipv4Network minuend = Ipv4Network.fromCidrNotation("192.168.0.0/16");
        final Ipv4Network subtrahend = Ipv4Network.fromCidrNotation("192.168.0.0/17");
        final Ipv4Network expected = Ipv4Network.fromCidrNotation("192.168.128.0/17");
        Assert.assertEquals(Collections.singleton(expected), new Ipv4SubtractNetworksFromNetwork().perform(minuend, Collections.singleton(subtrahend)));
    }

    @Test
    public void canSubtractCidrFromItself() {
        final Ipv4Network cidr = Ipv4Network.fromCidrNotation("192.168.0.0/16");
        Assert.assertEquals(Collections.emptySet(), new Ipv4SubtractNetworksFromNetwork().perform(cidr, Collections.singleton(cidr)));
    }

    @Test
    public void subtractingNonOverlappingRangesYieldsMinuend() {
        final Ipv4Network minuend = Ipv4Network.fromCidrNotation("192.168.0.0/16");
        final Ipv4Network nonOverlappingSubtrahend = Ipv4Network.fromCidrNotation("10.0.0.0/8");
        Assert.assertEquals(Collections.singleton(minuend), new Ipv4SubtractNetworksFromNetwork().perform(minuend, Collections.singleton(nonOverlappingSubtrahend)));
    }

    @Test
    public void subtractingContainerFromContainedYieldsEmptySet() {
        final Ipv4Network minuend = Ipv4Network.fromCidrNotation("192.168.0.0/17");
        final Ipv4Network subtrahend = Ipv4Network.fromCidrNotation("192.168.0.0/16");
        Assert.assertEquals(Collections.emptySet(), new Ipv4SubtractNetworksFromNetwork().perform(minuend, Collections.singleton(subtrahend)));
    }

    @Test
    public void SubtractingSmallCidrFromBigCidrYieldsLotsOfCidrs() {
        final Ipv4Network minuend = Ipv4Network.fromCidrNotation("10.0.0.0/8");
        final Ipv4Network subtrahend = Ipv4Network.fromCidrNotation("10.0.0.0/16");
        final Set<Ipv4Network> expected = new HashSet<>(Arrays.asList(
                Ipv4Network.fromCidrNotation("10.1.0.0/16"),
                Ipv4Network.fromCidrNotation("10.2.0.0/15"),
                Ipv4Network.fromCidrNotation("10.4.0.0/14"),
                Ipv4Network.fromCidrNotation("10.8.0.0/13"),
                Ipv4Network.fromCidrNotation("10.16.0.0/12"),
                Ipv4Network.fromCidrNotation("10.32.0.0/11"),
                Ipv4Network.fromCidrNotation("10.64.0.0/10"),
                Ipv4Network.fromCidrNotation("10.128.0.0/9")));
        Assert.assertEquals(expected, new Ipv4SubtractNetworksFromNetwork().perform(minuend, Collections.singleton(subtrahend)));
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullMinuendThrows() {
        new Ipv4SubtractNetworksFromNetwork().perform(null, Collections.<Ipv4Network>emptySet());
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullSubtrahendThrows() {
        new Ipv4SubtractNetworksFromNetwork().perform(Ipv4Network.fromCidrNotation("10.0.0.0/8"), null);
    }
}
