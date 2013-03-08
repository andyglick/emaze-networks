package net.emaze.networks;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import org.junit.Assert;
import org.junit.Test;

public class SortCidrsByFirstThenLastIpTest {

    @Test
    public void sortingYieldsExpectedOrder() {
        final Collection<Cidr> unsorted = Arrays.asList(
                Cidr.parse("192.168.17.0/24"),
                Cidr.parse("192.168.0.0/24"),
                Cidr.parse("192.168.16.0/24"),
                Cidr.parse("192.168.1.0/24"));
        final List<Cidr> sorted = Arrays.asList(
                Cidr.parse("192.168.0.0/24"),
                Cidr.parse("192.168.1.0/24"),
                Cidr.parse("192.168.16.0/24"),
                Cidr.parse("192.168.17.0/24"));
        Assert.assertEquals(sorted, new SortCidrsByFirstThenLastIp().perform(unsorted));
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullCidrsThrows() {
        new SortCidrsByFirstThenLastIp().perform(null);
    }
}
