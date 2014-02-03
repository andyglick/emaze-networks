package net.emaze.networks.my;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import org.junit.Test;
import org.junit.Assert;

public class SubtractNetworksFromNetworkTest {

    @Test
    public void canSubtractContainedFromContainer() {
        final Network minuend = Network.fromCidrNotation("192.168.0.0/16");
        final Network subtrahend = Network.fromCidrNotation("192.168.0.0/17");
        final Network expected = Network.fromCidrNotation("192.168.128.0/17");
        Assert.assertEquals(Collections.singleton(expected), new SubtractNetworksFromNetwork().perform(minuend, Collections.singleton(subtrahend)));
    }

    @Test
    public void canSubtractCidrFromItself() {
        final Network cidr = Network.fromCidrNotation("192.168.0.0/16");
        Assert.assertEquals(Collections.emptySet(), new SubtractNetworksFromNetwork().perform(cidr, Collections.singleton(cidr)));
    }

    @Test
    public void subtractingNonOverlappingRangesYieldsMinuend() {
        final Network minuend = Network.fromCidrNotation("192.168.0.0/16");
        final Network nonOverlappingSubtrahend = Network.fromCidrNotation("10.0.0.0/8");
        Assert.assertEquals(Collections.singleton(minuend), new SubtractNetworksFromNetwork().perform(minuend, Collections.singleton(nonOverlappingSubtrahend)));
    }

    @Test
    public void subtractingContainerFromContainedYieldsEmptySet() {
        final Network minuend = Network.fromCidrNotation("192.168.0.0/17");
        final Network subtrahend = Network.fromCidrNotation("192.168.0.0/16");
        Assert.assertEquals(Collections.emptySet(), new SubtractNetworksFromNetwork().perform(minuend, Collections.singleton(subtrahend)));
    }

    @Test
    public void SubtractingSmallCidrFromBigCidrYieldsLotsOfCidrs() {
        final Network minuend = Network.fromCidrNotation("10.0.0.0/8");
        final Network subtrahend = Network.fromCidrNotation("10.0.0.0/16");
        final Set<Network> expected = new HashSet<>(Arrays.asList(
                Network.fromCidrNotation("10.1.0.0/16"),
                Network.fromCidrNotation("10.2.0.0/15"),
                Network.fromCidrNotation("10.4.0.0/14"),
                Network.fromCidrNotation("10.8.0.0/13"),
                Network.fromCidrNotation("10.16.0.0/12"),
                Network.fromCidrNotation("10.32.0.0/11"),
                Network.fromCidrNotation("10.64.0.0/10"),
                Network.fromCidrNotation("10.128.0.0/9")));
        Assert.assertEquals(expected, new SubtractNetworksFromNetwork().perform(minuend, Collections.singleton(subtrahend)));
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullMinuendThrows() {
        new SubtractNetworksFromNetwork().perform(null, Collections.<Network>emptySet());
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullSubtrahendThrows() {
        new SubtractNetworksFromNetwork().perform(Network.fromCidrNotation("10.0.0.0/8"), null);
    }

}