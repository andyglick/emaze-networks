package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import junit.framework.Assert;
import net.emaze.networks.Ipv6Mask;
import org.junit.Test;

public class Ipv6MaskToStringTest {

    @Test
    public void testIPv6MaskSerialization() throws IOException {
        final String expected = "{'netmask':115}".replace("'", "\"");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithIpv6Mask bwn = new BeanWithIpv6Mask();
        bwn.setNetmask(Ipv6Mask.net(115));
        final String got = mapper.writeValueAsString(bwn);
        Assert.assertEquals(expected, got);
    }
}
