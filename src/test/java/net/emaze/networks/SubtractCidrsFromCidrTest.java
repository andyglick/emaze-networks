package net.emaze.networks;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import org.junit.Assert;
import org.junit.Test;

public class SubtractCidrsFromCidrTest {

    @Test
    public void canSubtractContainedFromContainer() {
        final Cidr minuend = Cidr.parse("192.168.0.0/16");
        final Cidr subtrahend = Cidr.parse("192.168.0.0/17");
        final Cidr expected = Cidr.parse("192.168.128.0/17");
        Assert.assertEquals(Collections.singleton(expected), new SubtractCidrsFromCidr().perform(minuend, Collections.singleton(subtrahend)));
    }

    @Test
    public void canSubtractCidrFromItself() {
        final Cidr cidr = Cidr.parse("192.168.0.0/16");
        Assert.assertEquals(Collections.emptySet(), new SubtractCidrsFromCidr().perform(cidr, Collections.singleton(cidr)));
    }

    @Test
    public void subtractingNonOverlappingRangesYieldsMinuend() {
        final Cidr minuend = Cidr.parse("192.168.0.0/16");
        final Cidr nonOverlappingSubtrahend = Cidr.parse("10.0.0.0/8");
        Assert.assertEquals(Collections.singleton(minuend), new SubtractCidrsFromCidr().perform(minuend, Collections.singleton(nonOverlappingSubtrahend)));
    }

    @Test
    public void subtractingContainerFromContainedYieldsEmptySet() {
        final Cidr minuend = Cidr.parse("192.168.0.0/17");
        final Cidr subtrahend = Cidr.parse("192.168.0.0/16");
        Assert.assertEquals(Collections.emptySet(), new SubtractCidrsFromCidr().perform(minuend, Collections.singleton(subtrahend)));
    }
    
    @Test
    public void SubtractingSmallCidrFromBigCidrYieldsLotsOfCidrs() {
        final Cidr minuend = Cidr.parse("10.0.0.0/8");
        final Cidr subtrahend = Cidr.parse("10.0.0.0/16");
        final Set<Cidr> expected = new HashSet<>(Arrays.asList(
                Cidr.parse("10.1.0.0/16"),
                Cidr.parse("10.2.0.0/15"),
                Cidr.parse("10.4.0.0/14"),
                Cidr.parse("10.8.0.0/13"),
                Cidr.parse("10.16.0.0/12"),
                Cidr.parse("10.32.0.0/11"),
                Cidr.parse("10.64.0.0/10"),
                Cidr.parse("10.128.0.0/9")));
        Assert.assertEquals(expected, new SubtractCidrsFromCidr().perform(minuend, Collections.singleton(subtrahend)));
    }
    
    @Test(expected = IllegalArgumentException.class) 
    public void nullMinuendThrows() {
        new SubtractCidrsFromCidr().perform(null, Collections.<Cidr>emptySet());
    }
    
    @Test(expected = IllegalArgumentException.class) 
    public void nullSubtrahendThrows() {
        new SubtractCidrsFromCidr().perform(Cidr.parse("10.0.0.0/8"), null);
    }
}
